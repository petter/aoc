import numpy as np


padding = 41
points = []

with open('input.txt', 'r') as f:
    for line in f:
        points.append(tuple([int(x) for x in line.split(', ')]))


xcoords = [x for x, y in points]
ycoords = [y for x, y in points]
bounds = [(min(xcoords) - padding, min(ycoords) - padding),
          (max(xcoords) + padding + 1, max(ycoords) + padding + 1)]

print(bounds)
p_map = np.ones((bounds[1][1] - bounds[0][1],
                 bounds[1][0] - bounds[0][0]), dtype=int) * -1

for i, p in enumerate(points):
    p = (p[1] - bounds[0][1], p[0] - bounds[0][0])
    p_map[p] = i

print(p_map)


def manhattan_distance(p, q):
    dist = 0
    for i in range(len(p)):
        dist += abs(p[i] - q[i])

    return dist


for y in range(len(p_map)):
    for x in range(len(p_map[i])):
        if p_map[y, x] != -1:
            continue
        shortest_ids = []
        shortest_dist = 1000000
        for i, p in enumerate(points):
            cur_dist = manhattan_distance(p, (x, y))
            if cur_dist < shortest_dist:
                shortest_dist = cur_dist
                shortest_ids = [i]
            elif cur_dist == shortest_dist:
                shortest_ids.append(i)

        if len(shortest_ids) == 1:
            p_map[y, x] = shortest_ids[0]

contestors = list(range(len(points)))

print(p_map)


def remove_element(el):
    try:
        cur_i = contestors.index(el)
        contestors.pop(cur_i)
    except:
        pass


# Remove all contestors that are at edges
for i in range(len(p_map[0])):
    cur = p_map[0, i]
    remove_element(cur)

for i in range(len(p_map[0])):
    cur = p_map[-1, i]
    remove_element(cur)

for i in range(len(p_map)):
    cur = p_map[i, 0]
    remove_element(cur)

for i in range(len(p_map)):
    cur = p_map[i, -1]
    remove_element(cur)

print(contestors)


largest_area_id = -1
largest_area = 0

for i in contestors:
    boolarr = p_map == i
    cur_area = np.sum(boolarr)
    print(i, cur_area)
    if cur_area > largest_area:
        largest_area = cur_area
        largest_area_id = i
print("Total area:", len(p_map) * len(p_map[0]))
print("Largest area is :", largest_area, "Largest area id", largest_area_id)
np.savetxt("solution.txt", p_map, fmt='%i')
