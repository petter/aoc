class Day15 : Day {
    private fun parseInput(input: List<String>): List<List<GraphNode>> {
        val nodes = input
            .filter { it.trim().isNotEmpty() }
            .mapIndexed { y, row ->
                row.trim()
                    .split("")
                    .filter { it.isNotEmpty() }
                    .mapIndexed { x, cell -> GraphNode(cell.toInt(), y to x) }
            }
        return nodes
    }

    private fun assignNeighbours(nodes: List<List<GraphNode>>) {
        for(y in nodes.indices) {
            for(x in nodes[y].indices) {
                val node = nodes[y][x]
                if(y > 0) {
                    node.neighbours.add(nodes[y - 1][x])
                }
                if(x > 0) {
                    node.neighbours.add(nodes[y][x - 1])
                }
                if(y < nodes.size - 1) {
                    node.neighbours.add(nodes[y + 1][x])
                }
                if(x < nodes[y].size - 1) {
                    node.neighbours.add(nodes[y][x + 1])
                }
            }
        }
    }

    private fun makeMazeForPart2(nodes: List<List<GraphNode>>) : List<List<GraphNode>> {
        val res = mutableListOf<List<GraphNode>>()

        for (y in 0 until nodes.size * 5) {
            val row = mutableListOf<GraphNode>()
            res.add(row)
            for (x in 0 until nodes[0].size * 5) {
                if(y < nodes.size && x < nodes[y].size) {
                    row.add(nodes[y][x])
                } else {
                    val copyNode = if(y - nodes.size >= 0) {
                        res[y - nodes.size][x]
                    } else  {
                        res[y][x - nodes[y].size]
                    }

                    val newRisk = if(copyNode.risk == 9) 1 else copyNode.risk + 1
                    row.add(GraphNode(newRisk, y to x))
                }
            }
        }

        return res
    }

    // Dijkstra's algorithm
    private fun shortestPath(start: GraphNode, end: GraphNode, nodes: List<List<GraphNode>>): Int {
        start.distance = 0
        val queue = nodes.flatten().toMutableList()

        while(queue.isNotEmpty()) {
            val current = queue.minByOrNull { it.distance } ?: throw Error("No elements in queue")
            queue.remove(current)

            if(current == end) {
                continue
            }

            for(neighbour in current.neighbours) {
                val alt = current.distance + neighbour.risk
                if(alt < neighbour.distance) {
                    neighbour.distance = alt
                    neighbour.prev = current
                }
            }
        }

        return end.distance
    }

    private fun printMaze(nodes: List<List<GraphNode>>) {
        for(row in nodes) {
            for(node in row) {
                print(node.risk)
            }
            println()
        }
    }

    override fun part1(input: List<String>): String {
        val nodes = parseInput(input)
        assignNeighbours(nodes)
        val start = nodes.first().first()
        val end = nodes.last().last()
        return shortestPath(start, end, nodes).toString()
    }

    // 2569 too low
    override fun part2(input: List<String>): String {
        val smallNodes = parseInput(input)
        val nodes = makeMazeForPart2(smallNodes)
        assignNeighbours(nodes)
        val start = nodes.first().first()
        val end = nodes.last().last()
        return shortestPath(start, end, nodes).toString()
    }
}

private data class GraphNode(
    val risk: Int,
    val pos: Pair<Int, Int>,
    var prev: GraphNode? = null,
    var visited: Boolean = false,
    var distance: Int = Int.MAX_VALUE,
    var neighbours: MutableList<GraphNode> = mutableListOf()
)
