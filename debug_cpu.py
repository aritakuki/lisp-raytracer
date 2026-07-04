import math
# Camera
eyex, eyey, eyez = 550.0, -380.0, 650.0
lookx, looky, lookz = 0.0, 160.0, -1200.0
fx, fy, fz = lookx - eyex, looky - eyey, lookz - eyez
flen = math.sqrt(fx*fx + fy*fy + fz*fz)
fx, fy, fz = fx/flen, fy/flen, fz/flen

upx, upy, upz = 0.0, -1.0, 0.0
rx = fy*upz - fz*upy
ry = fz*upx - fx*upz
rz = fx*upy - fy*upx
rlen = math.sqrt(rx*rx + ry*ry + rz*rz)
rx, ry, rz = rx/rlen, ry/rlen, rz/rlen

ux = ry*fz - rz*fy
uy = rz*fx - rx*fz
uz = rx*fy - ry*fx

scale = math.tan(28.0 * math.pi / 360.0)

cx, cy, cz, r = 0.0, -300.0, -1200.0, 200.0

print(f"Fwd: {fx}, {fy}, {fz}")
print(f"Right: {rx}, {ry}, {rz}")
print(f"Up: {ux}, {uy}, {uz}")
print(f"Scale: {scale}")

u, v = 0.0, 0.0
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
print(f"Center ray disc: {disc}")
