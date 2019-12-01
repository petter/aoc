import re

num_workers = 5
nodes = {}
file_pattern = r'Step (\w) must be finished before step (\w) can begin.'
exec_order = ''


class Node:
    def __init__(self, id):
        self.id = id
        self.inedges = []
        self.outedges = []
        self.time = 60 + ord(id) - 64


def new_task(task_list):
    if task_list == []:
        return None
    c = min(task_list)
    task_list.pop(task_list.index(c))
    return nodes[c]


class Worker:
    def __init__(self, task_list):
        self.task = None
        self.task_list = task_list

    def get_task(self):
        if self.task is None:
            self.task = new_task(self.task_list)
            if self.task is not None:
                print("Got new task:", self.task.id, self.task.time)

    def tick(self):
        if self.task == None:
            return False

        self.task.time -= 1

        print("Working on", self.task.id, "Time left:", self.task.time)
        if self.task.time == 0:
            for neighbour in self.task.outedges:
                neighbour.inedges.pop(neighbour.inedges.index(self.task))
                if neighbour.inedges == []:
                    self.task_list.append(neighbour.id)

            self.task = None

        return True


with open('input.txt', 'r') as f:
    for line in f:
        m = re.match(file_pattern, line)

        for c in m.groups():
            if not c in nodes:
                nodes[c] = Node(c)

        nodes[m.group(1)].outedges.append(nodes[m.group(2)])
        nodes[m.group(2)].inedges.append(nodes[m.group(1)])

no_inedges = []
for c, n in nodes.items():
    if n.inedges == []:
        no_inedges.append(c)

worker = []
for i in range(num_workers):
    worker.append(Worker(no_inedges))

print("Num workers:", len(worker))

time = 0

while True:
    working = False
    print("-------------------------------------------\n", "Time:", time)

    for i, w in enumerate(worker):

        w.get_task()

    for i, w in enumerate(worker):
        cur_worker_status = w.tick()
        working = working or cur_worker_status

    if not working:
        break

    time += 1

print(time)
