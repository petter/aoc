import numpy as np
import re

fabric = np.zeros([1000,1000], dtype=int)
n_overlap = 0

with open('input.txt', 'r') as f:
    for line in f:
        
        matches = re.match(r'#\d+ @ (\d+),(\d+): (\d+)x(\d+)', line)
        x = int(matches.group(1))
        y = int(matches.group(2))
        width = int(matches.group(3))
        height = int(matches.group(4))

        for f_y in range(height):
            for f_x in range(width):
                if fabric[y + f_y][x + f_x] == 0:
                    fabric[y + f_y][x + f_x] = 1
                    
                elif fabric[y + f_y][x + f_x] == 1:
                    n_overlap += 1
                    fabric[y + f_y][x + f_x] += 1
                
                    


print(n_overlap)