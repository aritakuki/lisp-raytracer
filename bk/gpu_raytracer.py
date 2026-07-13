import torch
import numpy as np
import time
from PIL import Image

def run_raytracer(resolution=800, output_filename="spheres_gpu.png", device_type=None):
    if device_type is None:
        device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    else:
        device = torch.device(device_type)
        
    print(f"Using device: {device}")
    
    # ----------------------------------------------------
    # Scene Setup
    # ----------------------------------------------------
    eye = torch.tensor([550.0, -380.0, 650.0], device=device, dtype=torch.float64)
    lookat = torch.tensor([0.0, 160.0, -1200.0], device=device, dtype=torch.float64)
    up = torch.tensor([0.0, -1.0, 0.0], device=device, dtype=torch.float64)
    fov_deg = 28.0
    
    # Light source
    light_center = torch.tensor([600.0, 300.0, 200.0], device=device, dtype=torch.float64)
    light_radius = 45.0
    shadow_mul = 0.75
    ambient = 0.25
    
    # Floor plane
    plane_p0 = torch.tensor([0.0, 500.0, -1400.0], device=device, dtype=torch.float64)
    plane_normal = torch.tensor([0.0, -1.0, 0.0], device=device, dtype=torch.float64)
    plane_half_size = 2500.0
    plane_checker_size = 140.0
    plane_color1 = torch.tensor([0.9, 0.9, 0.9], device=device, dtype=torch.float64)
    plane_color2 = torch.tensor([0.2, 0.2, 0.2], device=device, dtype=torch.float64)
    plane_reflectivity = 0.05
    
    # Spheres
    sphere_centers = []
    sphere_radii = []
    sphere_colors = []
    sphere_reflectivities = []
    
    # Large spheres
    sphere_centers.append([0.0, -300.0, -1200.0])
    sphere_radii.append(200.0)
    sphere_colors.append([0.8, 0.2, 0.2])
    sphere_reflectivities.append(0.02)
    
    sphere_centers.append([-80.0, -150.0, -1200.0])
    sphere_radii.append(200.0)
    sphere_colors.append([0.2, 0.8, 0.2])
    sphere_reflectivities.append(0.2)
    
    sphere_centers.append([70.0, -100.0, -1200.0])
    sphere_radii.append(200.0)
    sphere_colors.append([0.2, 0.2, 0.9])
    sphere_reflectivities.append(0.2)
    
    # Small spheres (deterministically random colors matching original code flow)
    torch.manual_seed(42)
    for x in range(-2, 3):
        for z in range(2, 8):
            sphere_centers.append([float(x * 200), 300.0, float(z * -400)])
            sphere_radii.append(40.0)
            sphere_colors.append(torch.rand(3).tolist())
            sphere_reflectivities.append(0.1)
            
    sphere_centers = torch.tensor(sphere_centers, device=device, dtype=torch.float64) # (M, 3)
    sphere_radii = torch.tensor(sphere_radii, device=device, dtype=torch.float64) # (M,)
    sphere_colors = torch.tensor(sphere_colors, device=device, dtype=torch.float64) # (M, 3)
    sphere_reflectivities = torch.tensor(sphere_reflectivities, device=device, dtype=torch.float64) # (M,)
    num_spheres = sphere_centers.shape[0]
    
    # ----------------------------------------------------
    # Camera Ray Generation
    # ----------------------------------------------------
    f = lookat - eye
    f = f / torch.linalg.norm(f)
    
    r = torch.cross(f, up)
    r = r / torch.linalg.norm(r)
    
    u = torch.cross(r, f)
    u = u / torch.linalg.norm(u)
    
    scale = torch.tan(torch.tensor(0.5 * fov_deg * np.pi / 180.0, device=device, dtype=torch.float64))
    
    # Sky range calculation matching Lisp update-sky-range
    ray1_dir = f + u * 1.0 * scale
    ray1_dir = ray1_dir / torch.linalg.norm(ray1_dir)
    yr1 = ray1_dir[1].item()
    
    ray2_dir = f
    ray2_dir = ray2_dir / torch.linalg.norm(ray2_dir)
    yr2 = ray2_dir[1].item()
    
    sky_yr_min = min(yr1, yr2)
    sky_yr_max = max(yr1, yr2)
    
    # Pixel grids
    H = W = resolution
    iy = torch.arange(H, device=device, dtype=torch.float64)
    ix = torch.arange(W, device=device, dtype=torch.float64)
    grid_y, grid_x = torch.meshgrid(iy, ix, indexing='ij')
    
    invn = 1.0 / H
    sy = 1.0 - 2.0 * (grid_y + 0.5) * invn
    sx = 2.0 * (grid_x + 0.5) * invn - 1.0
    
    sx_flat = sx.flatten() # (P,)
    sy_flat = sy.flatten() # (P,)
    P = sx_flat.shape[0]
    
    ray_dir_flat = f.view(1, 3) + r.view(1, 3) * sx_flat.unsqueeze(-1) * scale + u.view(1, 3) * sy_flat.unsqueeze(-1) * scale
    ray_dir_flat = ray_dir_flat / torch.linalg.norm(ray_dir_flat, dim=-1, keepdim=True) # (P, 3)
    
    # Buffers
    pixel_colors = torch.zeros((P, 3), device=device, dtype=torch.float64)
    active_mask = torch.ones(P, dtype=torch.bool, device=device)
    
    ray_origins = eye.view(1, 3).repeat(P, 1) # (P, 3)
    ray_dirs = ray_dir_flat.clone() # (P, 3)
    throughput = torch.ones(P, device=device, dtype=torch.float64)
    
    # Vogel disk offset for soft shadows
    num_samples = 64
    golden_angle = 2.399963229728653
    sample_indices = torch.arange(num_samples, device=device, dtype=torch.float64)
    r_vogel = light_radius * torch.sqrt((sample_indices + 0.5) / num_samples)
    theta_vogel = sample_indices * golden_angle
    dx_vogel = r_vogel * torch.cos(theta_vogel)
    dz_vogel = r_vogel * torch.sin(theta_vogel)
    
    light_positions = light_center.unsqueeze(0).repeat(num_samples, 1)
    light_positions[:, 0] += dx_vogel
    light_positions[:, 2] += dz_vogel
    
    # Iterative ray tracing loop
    max_depth = 3
    start_time = time.time()
    
    for depth in range(max_depth):
        active_indices = torch.where(active_mask)[0]
        if len(active_indices) == 0:
            break
            
        O = ray_origins[active_indices] # (Pa, 3)
        D = ray_dirs[active_indices] # (Pa, 3)
        Pa = O.shape[0]
        
        # 1. Sphere Intersections
        O_exp = O.unsqueeze(1) # (Pa, 1, 3)
        D_exp = D.unsqueeze(1) # (Pa, 1, 3)
        C_exp = sphere_centers.unsqueeze(0) # (1, M, 3)
        R_exp = sphere_radii.unsqueeze(0) # (1, M)
        
        V = O_exp - C_exp # (Pa, M, 3)
        d1 = (V * D_exp).sum(dim=-1) # (Pa, M)
        d2 = (V * V).sum(dim=-1) - R_exp**2 # (Pa, M)
        disc = d1**2 - d2 # (Pa, M)
        
        t_sphere = torch.full((Pa, num_spheres), float('inf'), device=device, dtype=torch.float64)
        
        mask_disc = disc >= 0
        sqrt_disc = torch.sqrt(torch.clamp(disc, min=0.0))
        t1 = -d1 - sqrt_disc
        t2 = -d1 + sqrt_disc
        
        t1_valid = mask_disc & (t1 > 0.001)
        t2_valid = mask_disc & (~t1_valid) & (t2 > 0.001)
        
        t_sphere[t1_valid] = t1[t1_valid]
        t_sphere[t2_valid] = t2[t2_valid]
        
        min_t_spheres, sphere_indices = t_sphere.min(dim=-1) # (Pa,), (Pa,)
        
        # 2. Plane Intersection
        den = (D * plane_normal).sum(dim=-1) # (Pa,)
        vx = plane_p0 - O # (Pa, 3)
        num = (vx * plane_normal).sum(dim=-1) # (Pa,)
        
        t_plane = torch.full((Pa,), float('inf'), device=device, dtype=torch.float64)
        valid_den = den.abs() > 1e-8
        hit_t = num / den
        
        # Check planar boundary limits
        ix_p = O[:, 0] + hit_t * D[:, 0]
        iz_p = O[:, 2] + hit_t * D[:, 2]
        within_bounds = (hit_t > 0.001) & \
                        ((ix_p - plane_p0[0]).abs() <= plane_half_size) & \
                        ((iz_p - plane_p0[2]).abs() <= plane_half_size)
                        
        t_plane[valid_den & within_bounds] = hit_t[valid_den & within_bounds]
        
        # 3. Choose closest intersection
        closest_t = torch.minimum(min_t_spheres, t_plane) # (Pa,)
        hit_mask = closest_t < 1e9 # (Pa,)
        
        # No hit -> background sky color
        no_hit_indices = active_indices[~hit_mask]
        if len(no_hit_indices) > 0:
            yr_bg = ray_dirs[no_hit_indices, 1]
            sky_t = torch.clamp((yr_bg - sky_yr_min) / (sky_yr_max - sky_yr_min), 0.0, 1.0)
            sky_t = sky_t ** 0.3
            
            sky_r = (1.0 - sky_t) * 1.0 + sky_t * 0.2
            sky_g = (1.0 - sky_t) * 1.0 + sky_t * 0.5
            sky_b = (1.0 - sky_t) * 1.0 + sky_t * 1.0
            sky_color = torch.stack([sky_r, sky_g, sky_b], dim=-1) # (P_bg, 3)
            
            if depth == 0:
                pixel_colors[no_hit_indices] += sky_color
            else:
                lum = 0.333 * sky_color.sum(dim=-1)
                pixel_colors[no_hit_indices] += (throughput[no_hit_indices] * lum).unsqueeze(-1)
                
            active_mask[no_hit_indices] = False
            
        # Hit -> compute surface properties and shading
        hit_indices = active_indices[hit_mask]
        if len(hit_indices) > 0:
            Pa_hit = hit_indices.shape[0]
            
            t_val = closest_t[hit_mask] # (Pa_hit,)
            O_hit = O[hit_mask] # (Pa_hit, 3)
            D_hit = D[hit_mask] # (Pa_hit, 3)
            
            # Intersection point
            int_pt = O_hit + t_val.unsqueeze(-1) * D_hit # (Pa_hit, 3)
            
            # Identify object type
            is_sphere = (t_val == min_t_spheres[hit_mask])
            is_plane = ~is_sphere
            
            # Normals
            xn = torch.zeros((Pa_hit, 3), device=device, dtype=torch.float64)
            
            # Sphere normal
            sphere_idx_hit = sphere_indices[hit_mask][is_sphere]
            c_sph = sphere_centers[sphere_idx_hit]
            r_sph = sphere_radii[sphere_idx_hit]
            xn[is_sphere] = (int_pt[is_sphere] - c_sph) / r_sph.unsqueeze(-1)
            
            # Plane normal
            xn[is_plane] = plane_normal
            
            # Surface colors and reflectivity
            color_surf = torch.zeros((Pa_hit, 3), device=device, dtype=torch.float64)
            refl_surf = torch.zeros(Pa_hit, device=device, dtype=torch.float64)
            
            # Sphere properties
            color_surf[is_sphere] = sphere_colors[sphere_idx_hit]
            refl_surf[is_sphere] = sphere_reflectivities[sphere_idx_hit]
            
            # Plane properties
            if is_plane.any():
                ix_coords = torch.floor((int_pt[is_plane, 0] - plane_p0[0]) / plane_checker_size).to(torch.int32)
                iz_coords = torch.floor((int_pt[is_plane, 2] - plane_p0[2]) / plane_checker_size).to(torch.int32)
                is_even = ((ix_coords + iz_coords) % 2) == 0
                color_surf[is_plane] = torch.where(is_even.unsqueeze(-1), plane_color1, plane_color2)
                refl_surf[is_plane] = plane_reflectivity
                
            # Soft shadow factor calculation (64 samples loop to save memory)
            shadowed_sum = torch.zeros(Pa_hit, device=device, dtype=torch.float64)
            
            for s_idx in range(num_samples):
                lp = light_positions[s_idx] # (3,)
                to_light = lp - int_pt # (Pa_hit, 3)
                dist_light = torch.linalg.norm(to_light, dim=-1) # (Pa_hit,)
                lx_light = to_light / dist_light.unsqueeze(-1)
                
                # Offset intersection point to avoid self-shadowing
                eps = 0.0005 * dist_light
                offset_pt = int_pt + xn * eps.unsqueeze(-1)
                dist_light_offset = torch.linalg.norm(lp - offset_pt, dim=-1)
                
                # Check intersections with all objects along the shadow ray
                # 1) Spheres shadow check
                offset_exp = offset_pt.unsqueeze(1)
                lx_exp = lx_light.unsqueeze(1)
                
                V_sh = offset_exp - sphere_centers.unsqueeze(0)
                d1_sh = (V_sh * lx_exp).sum(dim=-1)
                d2_sh = (V_sh * V_sh).sum(dim=-1) - sphere_radii.unsqueeze(0)**2
                disc_sh = d1_sh**2 - d2_sh
                
                t_sh_sph = torch.full((Pa_hit, num_spheres), float('inf'), device=device, dtype=torch.float64)
                mask_disc_sh = disc_sh >= 0
                sqrt_disc_sh = torch.sqrt(torch.clamp(disc_sh, min=0.0))
                t1_sh = -d1_sh - sqrt_disc_sh
                t2_sh = -d1_sh + sqrt_disc_sh
                
                t1_sh_valid = mask_disc_sh & (t1_sh > 0.05)
                t2_sh_valid = mask_disc_sh & (~t1_sh_valid) & (t2_sh > 0.05)
                
                t_sh_sph[t1_sh_valid] = t1_sh[t1_sh_valid]
                t_sh_sph[t2_sh_valid] = t2_sh[t2_sh_valid]
                
                # Ignore the sphere that we hit
                ignore_sphere_mask = is_sphere.unsqueeze(-1).repeat(1, num_spheres)
                hit_sph_idx = sphere_indices[hit_mask].unsqueeze(-1).repeat(1, num_spheres)
                range_sph = torch.arange(num_spheres, device=device).unsqueeze(0).repeat(Pa_hit, 1)
                ignore_mask = ignore_sphere_mask & (hit_sph_idx == range_sph)
                t_sh_sph[ignore_mask] = float('inf')
                
                blocked_sph = (t_sh_sph < dist_light_offset.unsqueeze(-1)).any(dim=-1) # (Pa_hit,)
                
                # 2) Plane shadow check
                den_sh = (lx_light * plane_normal).sum(dim=-1)
                vx_sh = plane_p0 - offset_pt
                num_sh = (vx_sh * plane_normal).sum(dim=-1)
                t_sh_pl = torch.full((Pa_hit,), float('inf'), device=device, dtype=torch.float64)
                valid_den_sh = den_sh.abs() > 1e-8
                hit_t_sh = num_sh / den_sh
                
                ix_sh = offset_pt[:, 0] + hit_t_sh * lx_light[:, 0]
                iz_sh = offset_pt[:, 2] + hit_t_sh * lx_light[:, 2]
                
                within_bounds_sh = (hit_t_sh > 0.05) & \
                                   (hit_t_sh < dist_light_offset) & \
                                   ((ix_sh - plane_p0[0]).abs() <= plane_half_size) & \
                                   ((iz_sh - plane_p0[2]).abs() <= plane_half_size)
                                   
                t_sh_pl[valid_den_sh & within_bounds_sh] = hit_t_sh[valid_den_sh & within_bounds_sh]
                
                # Ignore plane if we hit the plane
                t_sh_pl[is_plane] = float('inf')
                
                blocked_pl = t_sh_pl < dist_light_offset
                
                blocked = blocked_sph | blocked_pl
                shadowed_sum += torch.where(blocked, shadow_mul, 1.0)
                
            sf = shadowed_sum / num_samples # (Pa_hit,)
            
            # Diffuse (Lambert)
            to_light_center = light_center - int_pt
            dist_lc = torch.linalg.norm(to_light_center, dim=-1)
            lx_lc = to_light_center / dist_lc.unsqueeze(-1)
            
            lambert_factor = torch.clamp((lx_lc * xn).sum(dim=-1), min=0.0) # (Pa_hit,)
            diff = sf * lambert_factor
            
            # Specular
            dot_ln = (-lx_lc * xn).sum(dim=-1, keepdim=True)
            rx = -lx_lc - 2.0 * dot_ln * xn
            rx = rx / torch.linalg.norm(rx, dim=-1, keepdim=True)
            
            vdot = torch.clamp((rx * (-D_hit)).sum(dim=-1), min=0.0)
            specular_factor = vdot ** 8
            spec = 1.5 * sf * specular_factor
            
            # Base light amount
            base_light = ambient + 0.7 * diff + spec
            
            # Base Color scaled
            base_color = color_surf * base_light.unsqueeze(-1) # (Pa_hit, 3)
            
            # Refl factor (Fresnel approximation)
            vdot_refl = torch.clamp((-D_hit * xn).sum(dim=-1), min=0.0)
            refl = refl_surf + (1.0 - refl_surf) * ((1.0 - vdot_refl) ** 5)
            
            # Reflective ray setup for next depth
            dot_dn = (D_hit * xn).sum(dim=-1, keepdim=True)
            reflect_dir = D_hit - 2.0 * dot_dn * xn
            reflect_dir = reflect_dir / torch.linalg.norm(reflect_dir, dim=-1, keepdim=True)
            
            # Update output pixel color
            if depth == 0:
                pixel_colors[hit_indices] += base_color
                throughput[hit_indices] = refl
            else:
                # Reflection channels return luminance
                lum = 0.333 * base_color.sum(dim=-1)
                pixel_colors[hit_indices] += (throughput[hit_indices] * lum).unsqueeze(-1)
                throughput[hit_indices] = throughput[hit_indices] * refl
                
            # Set up next rays
            ray_origins[hit_indices] = int_pt + xn * 0.001
            ray_dirs[hit_indices] = reflect_dir
            
            # Terminate rays with low throughput or if reflectivity is 0
            active_mask[hit_indices] = (throughput[hit_indices] > 1e-4) & (refl > 0.0)
            
    print(f"Rendering finished in {time.time() - start_time:.4f} seconds.")
    
    # Format and save
    pixel_colors = torch.clamp(pixel_colors, 0.0, 1.0).cpu().numpy()
    img_data = np.round(pixel_colors.reshape((H, W, 3)) * 255.0).astype(np.uint8)
    
    img = Image.fromarray(img_data)
    img.save(output_filename)
    print(f"Saved rendered image to {output_filename}")

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description="GPU Raytracer in PyTorch")
    parser.add_argument('--res', type=int, default=8, help="Resolution multiplier (default: 8 -> 800x800)")
    parser.add_argument('--device', type=str, default=None, help="Device to run on (cuda or cpu)")
    parser.add_argument('--out', type=str, default="spheres_gpu.png", help="Output filename")
    args = parser.parse_args()
    
    run_raytracer(resolution=args.res * 100, output_filename=args.out, device_type=args.device)
