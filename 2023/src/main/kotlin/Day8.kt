class Day8 : Day {
    private fun parseInput(input: List<String>): Pair<String, Map<String, Node>> {
        val instructions = input.first()
        val nodeDescriptions = input.drop(2).map { line ->
            val (nodeName, nodeEdges) = line.split(" = ")
            val (left, right) = nodeEdges.split(", ")
            nodeName to (left.drop(1) to right.dropLast(1))
        }

        val nodes = nodeDescriptions.associate { (nodeName) -> nodeName to Node(nodeName) }
        nodeDescriptions.forEach { (nodeName, edges)  ->
            val node = nodes[nodeName]!!
            node.left = nodes[edges.first]
            node.right = nodes[edges.second]
        }

        return instructions to nodes
    }

    private fun walkUntil(instructions: String, startNode: Node, condition: (Node) -> Boolean): Long {
        var curNode = startNode
        var steps = 0L

        while (!condition(curNode)) {
            curNode = when (val instruction = instructions[(steps % instructions.length).toInt()]) {
                'L' -> curNode.left!!
                'R' -> curNode.right!!
                else -> throw Error("Unknown instruction $instruction")
            }
            steps++
        }

        return steps
    }

    override fun part1(input: List<String>): String {
        val (instructions, nodes) = parseInput(input)
        val startNode = nodes["AAA"] ?: throw Error("Could not find start node")
        val steps = walkUntil(instructions, startNode) { it.name == "ZZZ" }
        return steps.toString()
    }

    override fun part2(input: List<String>): String {
        val (instructions, nodes) = parseInput(input)
        val startNodes = nodes.values.filter { it.name.last() == 'A' }
        val stepsForEach = startNodes.map { walkUntil(instructions, it) { node -> node.name.last() == 'Z' } }
        val minSteps = stepsForEach.reduce(::leastCommonMultiple)
        return minSteps.toString()
    }

}

private data class Node(val name: String) {
    var left: Node? = null
    var right: Node? = null
}