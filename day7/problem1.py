import re


class Node:
    def __init__(self, id):
        self.id = id
        self.inedges = []
        self.outedges = []


nodes = {}
file_pattern = r'Step (\w) must be finished before step (\w) can begin.'

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

exec_order = ''


while no_inedges != []:
    print(no_inedges)
    c = min(no_inedges)
    print(c)
    no_inedges.pop(no_inedges.index(c))

    exec_order += c

    c_node = nodes[c]
    for neighbour in c_node.outedges:
        neighbour.inedges.pop(neighbour.inedges.index(c_node))
        if neighbour.inedges == []:
            no_inedges.append(neighbour.id)

print(exec_order)
