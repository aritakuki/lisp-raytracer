import math

# ============================================================
# Camera setup (shared)
# ============================================================
eyex, eyey, eyez = 550.0, -380.0, 650.0
lookx, looky, lookz = 0.0, 160.0, -1200.0
upx, upy, upz = 0.0, -1.0, 0.0
fov_deg = 28.0

# Forward
fx, fy, fz = lookx - eyex, looky - eyey, lookz - eyez
flen = math.sqrt(fx*fx + fy*fy + fz*fz)
fx, fy, fz = fx/flen, fy/flen, fz/flen

# Right = F x Up
rx = fy*upz - fz*upy
ry = fz*upx - fx*upz
rz = fx*upy - fy*upx
rlen = math.sqrt(rx*rx + ry*ry + rz*rz)
rx, ry, rz = rx/rlen, ry/rlen, rz/rlen

# Up = R x F
ux = ry*fz - rz*fy
uy = rz*fx - rx*fz
uz = rx*fy - ry*fx

scale = math.tan(0.5 * fov_deg * math.pi / 180.0)

print(f"Forward: ({fx:.6f}, {fy:.6f}, {fz:.6f})")
print(f"Right:   ({rx:.6f}, {ry:.6f}, {rz:.6f})")
print(f"Up:      ({ux:.6f}, {uy:.6f}, {uz:.6f})")
print(f"Scale:   {scale:.6f}")

# ============================================================
# CPU camera-ray function (from ray-render.lsp lines 43-70)
# ============================================================
# camera-ray takes (x, y) where:
#   x = sx = 2*(ix+0.5)/n - 1     (line 22)
#   y = sy = 1 - 2*(iy+0.5)/n     (line 20)
# Then:
#   sx_cam = x * scale
#   sy_cam = y * scale
#   dir = F + R*sx_cam + U*sy_cam  (line 62-69)

def cpu_ray(ix, iy, n):
    """CPU camera ray for pixel (ix, iy)"""
    invn = 1.0 / n
    sy = 1.0 - 2.0 * (iy + 0.5) * invn  # line 20
    sx = 2.0 * (ix + 0.5) * invn - 1.0   # line 22
    
    sx_cam = sx * scale
    sy_cam = sy * scale
    
    dirx = fx + rx*sx_cam + ux*sy_cam
    diry = fy + ry*sx_cam + uy*sy_cam
    dirz = fz + rz*sx_cam + uz*sy_cam
    dlen = math.sqrt(dirx*dirx + diry*diry + dirz*dirz)
    return dirx/dlen, diry/dlen, dirz/dlen

# ============================================================
# GPU camera-ray function (from kernel lines 75-86)
# ============================================================
# GPU uses:
#   sx = 2*(ix+0.5)/w - 1     (line 75)
#   sy = 1 - 2*(iy+0.5)/h     (line 76)
# Then:
#   dir = F + R*sx*scale + U*sy*scale  (lines 79-81)

def gpu_ray(ix, iy, w, h):
    """GPU camera ray for pixel (ix, iy)"""
    inv_w = 1.0 / w
    inv_h = 1.0 / h
    sx = 2.0 * (ix + 0.5) * inv_w - 1.0   # line 75
    sy = 1.0 - 2.0 * (iy + 0.5) * inv_h   # line 76
    
    dirx = fx + rx*sx*scale + ux*sy*scale
    diry = fy + ry*sx*scale + uy*sy*scale
    dirz = fz + rz*sx*scale + uz*sy*scale
    dlen = math.sqrt(dirx*dirx + diry*diry + dirz*dirz)
    return dirx/dlen, diry/dlen, dirz/dlen

# ============================================================
# Compare rays for same pixel
# ============================================================
n = 100  # res=1 → 100x100
for (ix, iy) in [(0,0), (50,50), (99,99), (0,99), (99,0)]:
    cpu_d = cpu_ray(ix, iy, n)
    gpu_d = gpu_ray(ix, iy, n, n)
    match = all(abs(a-b) < 1e-10 for a, b in zip(cpu_d, gpu_d))
    if not match:
        print(f"\nMISMATCH at ({ix},{iy}):")
        print(f"  CPU: ({cpu_d[0]:.10f}, {cpu_d[1]:.10f}, {cpu_d[2]:.10f})")
        print(f"  GPU: ({gpu_d[0]:.10f}, {gpu_d[1]:.10f}, {gpu_d[2]:.10f})")
    else:
        print(f"Pixel ({ix},{iy}): MATCH ✓")

# ============================================================
# Check sky range
# ============================================================
# CPU: update-sky-range calls camera-ray(0, 1) and camera-ray(0, -1)
# This gives the Y component of rays at screen top and bottom

cpu_ray_top = cpu_ray(50, 0, n)      # top row
cpu_ray_bot = cpu_ray(50, 99, n)     # bottom row
cam_ray_01 = lambda: None  # camera-ray(0.0, 1.0)
# sx=0, sy=1
sx0, sy1 = 0.0, 1.0
d_x = fx + rx*sx0*scale + ux*sy1*scale
d_y = fy + ry*sx0*scale + uy*sy1*scale
d_z = fz + rz*sx0*scale + uz*sy1*scale
dlen = math.sqrt(d_x*d_x + d_y*d_y + d_z*d_z)
yr1 = d_y / dlen

sx0, symi1 = 0.0, -1.0
d_x = fx + rx*sx0*scale + ux*symi1*scale
d_y = fy + ry*sx0*scale + uy*symi1*scale
d_z = fz + rz*sx0*scale + uz*symi1*scale
dlen = math.sqrt(d_x*d_x + d_y*d_y + d_z*d_z)
yr2 = d_y / dlen

cpu_sky_min = min(yr1, yr2)
cpu_sky_max = max(yr1, yr2)

# GPU sky range (from host code lines 433-446):
# ray1 = F + U*1.0*scale, yr1 = ray1_y / |ray1|
ray1_x = fx + ux*1.0*scale
ray1_y = fy + uy*1.0*scale
ray1_z = fz + uz*1.0*scale
ray1_len = math.sqrt(ray1_x*ray1_x + ray1_y*ray1_y + ray1_z*ray1_z)
gpu_yr1 = ray1_y / ray1_len

# ray2 = F, yr2 = F_y / |F|  (note: F is already normalized, so yr2 = fy)
ray2_x = fx
ray2_y = fy
ray2_z = fz
ray2_len = math.sqrt(ray2_x*ray2_x + ray2_y*ray2_y + ray2_z*ray2_z)
gpu_yr2 = ray2_y / ray2_len

gpu_sky_min = min(gpu_yr1, gpu_yr2)
gpu_sky_max = max(gpu_yr1, gpu_yr2)

print(f"\nCPU sky range: yr_min={cpu_sky_min:.6f}, yr_max={cpu_sky_max:.6f}")
print(f"GPU sky range: yr_min={gpu_sky_min:.6f}, yr_max={gpu_sky_max:.6f}")

# ============================================================
# CPU intersection test (from util.lsp minroot)
# ============================================================
def cpu_minroot(a, b, c):
    disc = b*b - 4*a*c
    if disc < 0:
        return None
    sqrt_disc = math.sqrt(disc)
    t1 = (-b - sqrt_disc) / (2*a)
    t2 = (-b + sqrt_disc) / (2*a)
    eps = 0.001
    r1 = t1 if t1 > eps else None
    r2 = t2 if t2 > eps else None
    if r1 and r2: return min(r1, r2)
    if r1: return r1
    if r2: return r2
    return None

def cpu_sphere_intersect(sx, sy, sz, sr, ox, oy, oz, dx, dy, dz):
    """CPU sphere intersection (sphere.lsp lines 21-31)"""
    a = dx*dx + dy*dy + dz*dz
    b = 2 * ((ox-sx)*dx + (oy-sy)*dy + (oz-sz)*dz)
    c = (ox-sx)**2 + (oy-sy)**2 + (oz-sz)**2 - sr**2
    return cpu_minroot(a, b, c)

def gpu_sphere_intersect(sx, sy, sz, sr, ox, oy, oz, dx, dy, dz):
    """GPU sphere intersection (kernel lines 85-103)"""
    vx = ox - sx
    vy = oy - sy
    vz = oz - sz
    b_prime = vx*dx + vy*dy + vz*dz
    c_val = vx*vx + vy*vy + vz*vz - sr*sr
    disc = b_prime*b_prime - c_val
    if disc < 0:
        return None
    sqrt_disc = math.sqrt(disc)
    t1 = -b_prime - sqrt_disc
    t2 = -b_prime + sqrt_disc
    if t1 > 0.1: return t1
    if t2 > 0.1: return t2
    return None

# Test intersection with the 3 main spheres
spheres = [
    (0, -300, -1200, 200, "red"),
    (-80, -150, -1200, 200, "green"),
    (70, -100, -1200, 200, "blue"),
]

print("\n=== Intersection tests ===")
# Center pixel
dx, dy, dz = cpu_ray(50, 50, n)
for sx, sy, sz, sr, name in spheres:
    cpu_t = cpu_sphere_intersect(sx, sy, sz, sr, eyex, eyey, eyez, dx, dy, dz)
    gpu_t = gpu_sphere_intersect(sx, sy, sz, sr, eyex, eyey, eyez, dx, dy, dz)
    print(f"  {name}: CPU t={cpu_t}, GPU t={gpu_t}")

# ============================================================
# Check the critical difference: CPU uses minroot with 2a denominator
# vs GPU uses b' form (where b_prime = b/2)
# ============================================================
print("\n=== Intersection formula comparison ===")
# CPU: minroot(a, b, c) where a=1 (unit dir), b = 2*dot(V,D), c = dot(V,V) - r^2
# CPU t = (-b ± sqrt(b²-4ac)) / 2a
# GPU: b' = dot(V,D), c_val = dot(V,V) - r^2, disc = b'² - c_val
# GPU t = -b' ± sqrt(b'² - c_val)
# Note: b = 2*b', so CPU disc = (2b')² - 4*1*c = 4b'² - 4c = 4(b'² - c)
# CPU t = (-2b' ± sqrt(4(b'²-c))) / 2 = (-2b' ± 2*sqrt(b'²-c)) / 2 = -b' ± sqrt(b'²-c)
# So both formulas are equivalent! ✓

# BUT the epsilon check differs:
# CPU: t > 0.001
# GPU: t > 0.1
print("CPU epsilon: 0.001")
print("GPU epsilon: 0.1")
print("This could cause differences for glancing intersections near surface.")

# ============================================================
# Check the checkerboard pattern
# ============================================================
# CPU uses floor division: (floor (/ (- x px) checker-size))
# GPU uses sin-based: sin(pi/140 * x) * sin(pi/140 * (z - (-1400)))
print("\n=== Checkerboard pattern comparison ===")
# CPU: ix = floor((x - 0) / 140), iz = floor((z - (-1400)) / 140)
#   color = if (evenp (+ ix iz)) then color1 else color2
# GPU: val = sin(pi/140 * x) * sin(pi/140 * (z+1400))
#   if val > 0 then color1 else color2

# These should produce the same pattern. Let's verify:
for x in [10, 70, 140, 210]:
    for z in [-1400, -1260, -1120]:
        cpu_ix = math.floor(x / 140)
        cpu_iz = math.floor((z + 1400) / 140)
        cpu_even = (cpu_ix + cpu_iz) % 2 == 0
        gpu_val = math.sin(math.pi/140 * x) * math.sin(math.pi/140 * (z + 1400))
        gpu_even = gpu_val > 0
        if cpu_even != gpu_even:
            print(f"  MISMATCH at x={x}, z={z}: CPU={cpu_even}, GPU={gpu_even}")

# ============================================================
# Check: shadow epsilon comparison
# ============================================================
# CPU: shadowed-to-light uses eps = 0.0005 * dist, then blocked-to-light checks t > 0.05
# GPU: shadow uses eps = 0.0005 * sh-dist, then checks t1 > 0.1 (not 0.05!)
print("\n=== Shadow blocker epsilon ===")
print("CPU blocked-to-light: t > 0.05")
print("GPU shadow blocker:   t > 0.1")
print("This is a potential difference source!")

# ============================================================
# The big picture comparison: render a few sample pixels with full CPU logic
# and compare against what GPU would produce
# ============================================================
print("\n=== Top-left corner pixel values from CPU PPM ===")
with open('spheres.ppm', 'r') as f:
    header = f.readline()  # P3
    dims = f.readline().split()
    w, h = int(dims[0]), int(dims[1])
    f.readline()  # 255
    data = f.read().split()

# Print first few pixels from CPU rendering
for i in range(5):
    r, g, b = int(data[i*3]), int(data[i*3+1]), int(data[i*3+2])
    print(f"  CPU pixel ({i},0): R={r}, G={g}, B={b}")

# Print center pixels
center_start = (50 * w + 48) * 3
for i in range(5):
    idx = center_start + i * 3
    r, g, b = int(data[idx]), int(data[idx+1]), int(data[idx+2])
    print(f"  CPU pixel ({48+i},50): R={r}, G={g}, B={b}")

