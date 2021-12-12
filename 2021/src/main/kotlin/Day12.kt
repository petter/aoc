class Day12 : Day {
    override fun part1(input: List<String>): String {
        val graph = Graph(input, false)
        val paths = graph.findAllPaths()
        return paths.size.toString()
    }

    override fun part2(input: List<String>): String {
        val graph = Graph(input, true)
        val paths = graph.findAllPaths()
        return paths.size.toString()
    }
}

class Graph(input: List<String>, canVisitSmallCaveTwice: Boolean) {
    val allPaths = mutableListOf<String>()
    private val nodes = input.fold(mutableMapOf<String, Node>()) { map, line ->
        line.trim()
            .split("-")
            .toPair()
            .let { (a, b) ->
                val nodeA = map.getOrPut(a) { Node(a, canVisitSmallCaveTwice) }
                val nodeB = map.getOrPut(b) { Node(b, canVisitSmallCaveTwice) }
                nodeA.addEdge(nodeB)
                nodeB.addEdge(nodeA)
            }
        map
    }.toMap()

    fun findAllPaths(): List<String> {
        allPaths.clear()
        val startNode = nodes["start"] ?: throw IllegalStateException("No start node")
        val endNode = nodes["end"] ?: throw IllegalStateException("No end node")

        startNode.visit(endNode, mutableListOf(), this)
        return allPaths
    }
}


class Node(private val name: String, private val canVisitSmallCaveTwice: Boolean) {
    private val edges = mutableListOf<Node>()

    fun addEdge(node: Node) {
        edges.add(node)
    }

    fun visit(endNode: Node, path: MutableList<Node>, graph: Graph) {
        if (name == "start" && this in path) {
            return
        }

        if (this == endNode) {
            path.add(this)
            graph.allPaths.add(path.joinToString(",") { it.name })
            return
        }

        if (!canBeVisited(path)) {
            return
        }

        path.add(this)
        edges.forEach {
            it.visit(endNode, path.toMutableList(), graph)
        }
    }

    private fun canBeVisited(path: List<Node>): Boolean {
        return if(isBig()) {
            true
        } else if (this !in path) {
            true
        } else if (canVisitSmallCaveTwice) {
            !hasVisitedSmallCaveTwice(path)
        } else {
            false
        }
    }

    private fun isBig(): Boolean = name.all { it.isUpperCase() }

    private fun hasVisitedSmallCaveTwice(path: List<Node>): Boolean {
        val smallNodes = path.filter { !it.isBig() }
        val smallNodeSet = smallNodes.toSet()
        return smallNodes.size != smallNodeSet.size
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Node

        if (name != other.name) return false

        return true
    }

    override fun toString(): String {
        return name;
    }
}