import math

# Camera setup
eyex, eyey, eyez = 550.0, -380.0, 650.0
lookx, looky, lookz = 0.0, 160.0, -1200.0
upx, upy, upz = 0.0, -1.0, 0.0
fov_deg = 28.0

fx, fy, fz = lookx - eyex, looky - eyey, lookz - eyez
flen = math.sqrt(fx*fx + fy*fy + fz*fz)
fx, fy, fz = fx/flen, fy/flen, fz/flen

rx = fy*upz - fz*upy
ry = fz*upx - fx*upz
rz = fx*upy - fy*upx
rlen = math.sqrt(rx*rx + ry*ry + rz*rz)
rx, ry, rz = rx/rlen, ry/rlen, rz/rlen

ux = ry*fz - rz*fy
uy = rz*fx - rx*fz
uz = rx*fy - ry*fx

scale = math.tan(0.5 * fov_deg * math.pi / 180.0)

# CPU update-sky-range: calls camera-ray(0.0, 1.0) and camera-ray(0.0, -1.0)
# camera-ray(x, y): sx = x*scale, sy = y*scale, dir = F + R*sx + U*sy
# So camera-ray(0, 1) => dir = F + U*scale
# And camera-ray(0, -1) => dir = F - U*scale

# camera-ray(0, 1):
d1x = fx + ux * 1.0 * scale
d1y = fy + uy * 1.0 * scale
d1z = fz + uz * 1.0 * scale
d1len = math.sqrt(d1x*d1x + d1y*d1y + d1z*d1z)
yr1 = d1y / d1len

# camera-ray(0, -1):
d2x = fx + ux * (-1.0) * scale
d2y = fy + uy * (-1.0) * scale
d2z = fz + uz * (-1.0) * scale
d2len = math.sqrt(d2x*d2x + d2y*d2y + d2z*d2z)
yr2 = d2y / d2len

cpu_sky_min = min(yr1, yr2)
cpu_sky_max = max(yr1, yr2)

print(f"CPU camera-ray(0,1):  yr1 = {yr1:.10f}")
print(f"CPU camera-ray(0,-1): yr2 = {yr2:.10f}")
print(f"CPU sky range: [{cpu_sky_min:.10f}, {cpu_sky_max:.10f}]")

# GPU sky range calculation (run.sh lines 433-446):
# ray1 = F + U*1.0*scale  (same as CPU camera-ray(0,1))
# ray2 = F                (different! CPU uses camera-ray(0,-1) = F - U*scale)
gpu_ray1x = fx + ux * 1.0 * scale
gpu_ray1y = fy + uy * 1.0 * scale
gpu_ray1z = fz + uz * 1.0 * scale
gpu_ray1len = math.sqrt(gpu_ray1x**2 + gpu_ray1y**2 + gpu_ray1z**2)
gpu_yr1 = gpu_ray1y / gpu_ray1len

gpu_ray2x = fx  # THIS IS WRONG! Should be F - U*scale
gpu_ray2y = fy
gpu_ray2z = fz
gpu_ray2len = math.sqrt(gpu_ray2x**2 + gpu_ray2y**2 + gpu_ray2z**2)
gpu_yr2 = gpu_ray2y / gpu_ray2len

gpu_sky_min = min(gpu_yr1, gpu_yr2)
gpu_sky_max = max(gpu_yr1, gpu_yr2)

print(f"\nGPU ray1 (F+U*scale): yr1 = {gpu_yr1:.10f}")
print(f"GPU ray2 (F only!!!): yr2 = {gpu_yr2:.10f}")
print(f"GPU sky range: [{gpu_sky_min:.10f}, {gpu_sky_max:.10f}]")

# Correct GPU ray2 should be F - U*scale:
corr_ray2x = fx - ux * scale
corr_ray2y = fy - uy * scale
corr_ray2z = fz - uz * scale
corr_ray2len = math.sqrt(corr_ray2x**2 + corr_ray2y**2 + corr_ray2z**2)
corr_yr2 = corr_ray2y / corr_ray2len

corr_sky_min = min(gpu_yr1, corr_yr2)
corr_sky_max = max(gpu_yr1, corr_yr2)
print(f"\nCorrected ray2 (F-U*scale): yr2 = {corr_yr2:.10f}")
print(f"Corrected sky range: [{corr_sky_min:.10f}, {corr_sky_max:.10f}]")
print(f"Matches CPU? {abs(corr_sky_min - cpu_sky_min) < 1e-6 and abs(corr_sky_max - cpu_sky_max) < 1e-6}")

