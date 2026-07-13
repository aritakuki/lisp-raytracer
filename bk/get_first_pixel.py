import math

# Emulate GPU ray generation for pixel (0,0)
width = 100
height = 100
u = (0.5 - width/2.0) / height
v = (0.5 - height/2.0) / height

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

scale = math.tan(30.0 * math.pi / 360.0) # GPU fov

dirx = fx + u*rx*scale + v*ux*scale
diry = fy + u*ry*scale + v*uy*scale
dirz = fz + u*rz*scale + v*uz*scale
dlen = math.sqrt(dirx*dirx + diry*diry + dirz*dirz)
dirx, diry, dirz = dirx/dlen, diry/dlen, dirz/dlen

print("GPU Ray 0,0:", dirx, diry, dirz)

# sky color
dy = diry
sky_yr_min = -0.4 # approximate
sky_yr_max = -0.2
if dy < sky_yr_min: sky_t = 0.0
elif dy > sky_yr_max: sky_t = 1.0
else: sky_t = (dy - sky_yr_min) / (sky_yr_max - sky_yr_min)

sky_t_pow = sky_t ** 0.3
skyr = (1.0 - sky_t_pow)*1.0 + sky_t_pow*0.2
skyg = (1.0 - sky_t_pow)*1.0 + sky_t_pow*0.5
skyb = (1.0 - sky_t_pow)*1.0 + sky_t_pow*1.0

print("GPU Pixel 0,0:", int(skyr*255), int(skyg*255), int(skyb*255))
