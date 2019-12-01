import numpy as np
import re

fabric = np.zeros([1000,1000], dtype=int)
n_overlap = 0

claims = []

with open('input.txt', 'r') as f:
    for line in f:
        matches = re.match(r'#(\d+) @ (\d+),(\d+): (\d+)x(\d+)', line)
        id = int(matches.group(1))
        x = int(matches.group(2))
        y = int(matches.group(3))
        width = int(matches.group(4))
        height = int(matches.group(5))

        claims.append([id, x, y, width, height])



        for f_y in range(height):
            for f_x in range(width):
                if fabric[y + f_y][x + f_x] == 0:
                    fabric[y + f_y][x + f_x] = id
                elif fabric[y + f_y][x + f_x] != -1:
                    n_overlap += 1
                    fabric[y + f_y][x + f_x] = -1


for claim in claims:
    orig_area = claim[3] * claim[4]
    
    mask = fabric == claim[0]
    new_area = fabric[mask]
  

    if len(new_area) == orig_area:
        print(claim[0]) 


