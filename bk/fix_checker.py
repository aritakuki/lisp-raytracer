import math

# Analysis:
# sin(pi*x/W) is positive for floor(x/W) even, negative for floor(x/W) odd
# EXCEPT at boundaries: at x=0, sin=0 (treated as not positive by GPU)
# and at x=140, sin=0 (treated as not positive)
# Also for negative numbers:
# x=-10: floor(-10/140) = -1 (odd). sin(pi*-10/140) = -sin(pi*10/140) < 0. 
#   floor_even=False, sin_positive=False → both agree on "odd" → OK
# x=0: floor(0/140) = 0 (even). sin(0) = 0. 
#   floor_even=True, sin_positive=False → disagree! GPU gives color2, CPU gives color1
# x=140: floor(140/140) = 1 (odd). sin(pi) = 0.
#   floor_even=False, sin_positive=False → both agree on "odd" → OK (but GPU gives val=0 which is <=0)

# The main issue: at exact boundaries (x=0, z=-1400, multiples of 140)
# the sin goes to 0, causing GPU to always pick color2
# while CPU picks based on floor which can be either.
# But with floating-point hit positions, exact boundary hits are rare.

# HOWEVER, there's a more fundamental issue for x < 0:
# Let's check: x = -10
# CPU: floor(-10/140) = floor(-0.0714) = -1
# CPU: (evenp -1) = False → color2
# GPU: sin(pi * -10/140) = -0.2225 → negative → color2
# SAME!

# x = -150
# CPU: floor(-150/140) = floor(-1.071) = -2
# CPU: (evenp -2) = True → color1  
# GPU: sin(pi * -150/140) = sin(-pi*150/140) = sin(-pi - pi*10/140) = -sin(pi*10/140-pi) = sin(pi*10/140) > 0 → color1
# Wait: sin(pi*(-150)/140) = sin(-150*pi/140)
# -150/140 = -15/14 = -1.0714...
# sin(-1.0714*pi) = -sin(1.0714*pi) = -sin(pi + 0.0714*pi) = sin(0.0714*pi) = +0.2225
# So GPU: positive → color1. CPU: even(-2) → color1. SAME!

# The only mismatches are at exact boundaries. Let me check bulk:
mismatches = []
for x_int in range(-500, 500):
    for z_int in range(-2900, 100):
        x = x_int + 0.5  # avoid exact boundaries
        z = z_int + 0.5
        cpu_ix = math.floor(x / 140.0)
        cpu_iz = math.floor((z + 1400.0) / 140.0)
        cpu_even = (cpu_ix + cpu_iz) % 2 == 0
        
        gpu_val = math.sin(math.pi/140.0 * x) * math.sin(math.pi/140.0 * (z + 1400.0))
        gpu_even = gpu_val > 0
        
        if cpu_even != gpu_even:
            mismatches.append((x, z, cpu_ix, cpu_iz))

print(f"Mismatches with +0.5 offset: {len(mismatches)}")
if len(mismatches) > 0:
    print("First few:", mismatches[:5])
    
