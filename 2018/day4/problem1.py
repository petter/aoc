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
        
        if sleep[cur_guard]['sum'] > longest_sleeping_time:
            longest_sleeping_guard = cur_guard
            longest_sleeping_time = sleep[cur_guard]['sum']

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


most_slept = 0
min_most_slept = 0
for cur_min, slept in enumerate(sleep[longest_sleeping_guard]['sleeping_mins']):
    if slept > most_slept:
        most_slept = slept
        min_most_slept = cur_min

result = int(longest_sleeping_guard) * min_most_slept
print(result)