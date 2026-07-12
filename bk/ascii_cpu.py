import math
import sys

def print_cpu():
    width = 100
    height = 100
    
    # Camera
    eyex, eyey, eyez = 550.0, -380.0, 650.0
    lookx, looky, lookz = 0.0, 160.0, -1200.0

    fx, fy, fz = lookx - eyex, looky - eyey, lookz - eyez
    flen = math.sqrt(fx*fx + fy*fy + fz*fz)
    fx, fy, fz = fx/flen, fy/flen, fz/flen

    upx, upy, upz = 0.0, -1.0, 0.0
    # right = fwd x up
    rx = fy*upz - fz*upy
    ry = fz*upx - fx*upz
    rz = fx*upy - fy*upx
    rlen = math.sqrt(rx*rx + ry*ry + rz*rz)
    rx, ry, rz = rx/rlen, ry/rlen, rz/rlen

    # up = right x fwd
    ux = ry*fz - rz*fy
    uy = rz*fx - rx*fz
    uz = rx*fy - ry*fx

    scale = math.tan(28.0 * math.pi / 360.0)

    # Sphere 0
    cx, cy, cz, r = 0.0, -300.0, -1200.0, 200.0
    
    chars = " .:-=+*#%@"
    for y in range(0, height, 2):
        line = ""
        for x in range(0, width, 1):
            # CPU logic u, v
            u = (x + 0.5) / width - 0.5
            v = 0.5 - (y + 0.5) / height
            
            dirx = fx + u*rx*scale + v*ux*scale
            diry = fy + u*ry*scale + v*uy*scale
            dirz = fz + u*rz*scale + v*uz*scale
            dlen = math.sqrt(dirx*dirx + diry*diry + dirz*dirz)
            dx, dy, dz = dirx/dlen, diry/dlen, dirz/dlen
            
            vx = eyex - cx
            vy = eyey - cy
            vz = eyez - cz
            b = vx*dx + vy*dy + vz*dz
            c = vx*vx + vy*vy + vz*vz - r*r
            disc = b*b - c
            if disc >= 0:
                line += "@"
            else:
                line += "-"
        print(line)

print_cpu()
