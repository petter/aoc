import numpy as np


max_dist = 10000
padding = 40
points = []

with open('input.txt', 'r') as f:
    for line in f:
        points.append(tuple([int(x) for x in line.split(', ')]))


xcoords = [x for x, y in points]
ycoords = [y for x, y in points]
bounds = [(min(xcoords) - padding, min(ycoords) - padding),
          (max(xcoords) + padding + 1, max(ycoords) + padding + 1)]

p_map = np.zeros((bounds[1][1] - bounds[0][1],
                  bounds[1][0] - bounds[0][0]), dtype=int)


def manhattan_distance(p, q):
    dist = 0
    for i in range(len(p)):
        dist += abs(p[i] - q[i])

    return dist


for y in range(len(p_map)):
    for x in range(len(p_map[0])):
        sum_dist = 0
        for p in points:
            sum_dist += manhattan_distance(p, (x, y))

        if sum_dist < max_dist:
            p_map[y, x] = 1

area = np.sum(p_map)
print(area)

np.savetxt('solution2.txt', p_map, fmt='%i')
