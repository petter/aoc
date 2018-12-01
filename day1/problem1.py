

with open('input.txt', 'r') as f:
    res = 0
    
    for line in f:
        num = int(line[1:])
        if line[0] == '+':
            res += num
        else:
            res -= num

    print(res)