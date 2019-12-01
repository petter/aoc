import pandas as pd
import re

data = []
pattern = r'^\[(.*) (.*)\] (.*)$'

with open('input.txt', 'r') as f:
    for line in f:
        m = re.match(pattern, line)
        data.append(m.groups())

data = pd.DataFrame(data=data, columns=['Date', 'Time', 'Event'])
data = data.sort_values(['Date', 'Time'], ascending=[True, True])

sleep = {}
cur_guard = 0
sleep_start = 0
sleep_end = 0
guard_id_pattern = r'Guard #(\d*)'

longest_sleeping_time = 0
longest_sleeping_guard = 0

for index, row in data.iterrows():
    if row['Event'] == 'falls asleep':
        sleep_start = int(row['Time'][-2:])
    elif row['Event'] == 'wakes up':
        sleep_end = int(row['Time'][-2:])
        sleep[cur_guard]['sum'] += sleep_end - sleep_start

        for i in range(sleep_start, sleep_end):
            sleep[cur_guard]['sleeping_mins'][i] += 1
    else:
        m = re.match(guard_id_pattern, row['Event'])
        cur_guard = m.group(1)
        if cur_guard not in sleep:
            sleeping_mins = [0] * 60
            sleep[cur_guard] = {
                'sum': 0, 
                'sleeping_mins': sleeping_mins}

chosen_guard = 0
chosen_min = 0
highest_sleeping_min = 0
for guard in sleep:
    for cur_min, n in enumerate(sleep[guard]['sleeping_mins']):
        if n > highest_sleeping_min:
            chosen_min = cur_min
            highest_sleeping_min = n
            chosen_guard = guard

result = int(chosen_guard) * chosen_min
print(result)