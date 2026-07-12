import sys
def print_ppm(filename):
    with open(filename, 'r') as f:
        header = f.readline().strip()
        if header != 'P3': return
        dims = f.readline().strip().split()
        w, h = int(dims[0]), int(dims[1])
        f.readline() # maxval
        data = f.read().split()
        
    chars = " .:-=+*#%@"
    for y in range(0, h, 2):
        line = ""
        for x in range(0, w, 1):
            idx = (y * w + x) * 3
            if idx+2 >= len(data): break
            r, g, b = int(data[idx]), int(data[idx+1]), int(data[idx+2])
            lum = int(0.299*r + 0.587*g + 0.114*b)
            c = chars[lum * 9 // 255]
            line += c
        print(line)

print_ppm('spheres.ppm')
