with open('input.txt', 'r') as f:
    content = f.readlines()

for cur_line in content:
    for match_line in content:
        if cur_line == match_line:
            continue

        num_differ = 0
        s = cur_line
        for i in range(len(cur_line)):
            if cur_line[i] != match_line[i]:
                num_differ += 1
                s = s[:i] + s[i + 1:]

        if num_differ == 1:
            print(cur_line, match_line, s)