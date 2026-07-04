import math

# CPU checker pattern (plane.lsp lines 41-48):
# ix = floor((hit_x - plane_x) / checker_size)
# iz = floor((hit_z - plane_z) / checker_size)
# color = color1 if (evenp (+ ix iz)) else color2
# With plane at (0, 500, -1400) and checker_size=140

# GPU checker pattern (kernel lines 213-218):
# val = sin(pi/140 * hit_x) * sin(pi/140 * (hit_z - (-1400)))
# color = color1 if (val > 0) else color2

# These are fundamentally different:
# floor(x/140) gives you which "cell" you're in (integer division)
# sin(pi*x/140) changes sign at multiples of 140

# The sign of sin(pi*x/140) is:
#   positive when floor(x/140) is even
#   negative when floor(x/140) is odd
# So sin(pi*x1/140)*sin(pi*x2/140) > 0 when both floors are same parity
# which is the same as floor(...) + floor(...) being even.

# BUT: the CPU checker is (x - plane_x)/checker_size = (x - 0)/140 = x/140
# and (z - plane_z)/checker_size = (z - (-1400))/140 = (z + 1400)/140

# Let's check edge cases more carefully:
def cpu_checker(x, z):
    ix = math.floor(x / 140.0)
    iz = math.floor((z - (-1400.0)) / 140.0)
    return (ix + iz) % 2 == 0

def gpu_checker(x, z):
    val = math.sin(math.pi/140.0 * x) * math.sin(math.pi/140.0 * (z - (-1400.0)))
    return val > 0

# Test a grid
mismatches = 0
total = 0
for x in range(-500, 500, 10):
    for z in range(-2900, 100, 10):
        total += 1
        if cpu_checker(x, z) != gpu_checker(x, z):
            mismatches += 1

print(f"Mismatches: {mismatches} / {total} = {100*mismatches/total:.1f}%")

# The issue: for negative x, floor(x/140) differs from the sign of sin
# floor(-10/140) = floor(-0.071) = -1 (odd)
# sin(pi * -10 / 140) < 0 (same sign)
# So sign of sin matches parity of floor... let's check more carefully

for x in [-150, -10, 0, 10, 130, 140, 150, 280, 290]:
    cpu_ix = math.floor(x / 140.0)
    sin_val = math.sin(math.pi * x / 140.0)
    # sin > 0 when floor is even, sin < 0 when floor is odd? 
    print(f"x={x:4d}: floor(x/140)={cpu_ix:3d}, sin(pi*x/140)={sin_val:+.4f}, "
          f"floor_even={cpu_ix%2==0}, sin_positive={sin_val>0}")
    
