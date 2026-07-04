import math

def cpu_logic(hit_y=-330, nx=0.0, ny=-0.15, nz=0.99, dx=-0.31, dy=0.03, dz=-0.95):
    # Light at 600, 300, 200
    lx = 600 - 0
    ly = 300 - hit_y
    lz = 200 - (-1000)
    
    length = math.sqrt(lx*lx + ly*ly + lz*lz)
    lx /= length
    ly /= length
    lz /= length
    
    # Lambert
    lambert = max(0.0, lx*nx + ly*ny + lz*nz)
    sf = 0.75
    diff = sf * lambert
    
    # Specular
    lin_x = -lx
    lin_y = -ly
    lin_z = -lz
    
    dot_ln = lin_x*nx + lin_y*ny + lin_z*nz
    rx = lin_x - 2.0*dot_ln*nx
    ry = lin_y - 2.0*dot_ln*ny
    rz = lin_z - 2.0*dot_ln*nz
    
    vdot = max(0.0, rx*(-dx) + ry*(-dy) + rz*(-dz))
    spec = 1.5 * sf * (vdot ** 8)
    
    ambient = 0.25
    base = ambient + 0.7 * diff + spec
    
    return spec, base

print("CPU Logic:", cpu_logic())
