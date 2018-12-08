import numpy as np

exact_3 = 0
exact_2 = 0

with open('input.txt', 'r') as f:
    for line in f:
        chars = np.zeros(27)
        line = line.lower()
        for c in line:
            if(not c.isalpha()):
                continue
            print(ord(c) - 97, c)
            chars[ord(c) - 97] += 1

        if np.isin(chars, 3).any():
            exact_3 += 1

        if np.isin(chars, 2).any():
            exact_2 += 1

print(exact_3 * exact_2)

