enum class Direction(val num: Long) {
    North(1L),
    South(2L),
    West(3L),
    East(4L)
}

fun direction(from: Position, to: Position) : Direction =
    when {
        to.first < from.first -> Direction.West
        to.first > from.first -> Direction.East
        to.second < from.second -> Direction.North
        else -> Direction.South
    }

infix operator fun Pair<Int, Int>.plus(direction: Direction) : Pair<Int, Int> =
    when (direction) {
        Direction.North -> this.first     to this.second - 1
        Direction.South -> this.first     to this.second + 1
        Direction.East  -> this.first + 1 to this.second
        Direction.West  -> this.first - 1 to this.second
    }

class Maze() {
    val map = mutableMapOf<Position, Block>()

    companion object {
        fun fromString(s: String, blockIdentityFunction: (Char) -> Int) : Maze {
            val maze = Maze()
            s.lines().forEachIndexed { y, line -> line.forEachIndexed { x, c -> maze.addBlock(x to y, blockIdentityFunction(c)) } }
            return maze
        }
    }

    enum class BlockTypes {
        Wall,
        Path
    }

    fun addBlock(position: Position, type: Int) {
        val block = when (type) {
            0 -> Wall(position)
            1 -> Path(position)
            else -> error("Invalid block type $type")
        }

        map[position] = block

        val neighbors = findNeighbors(position)
        neighbors.forEach { it.neighbors.add(block) }
        block.neighbors.addAll(neighbors)
    }

    fun findShortestPath(from: Position, to: Position) : List<Direction>? = map[from]?.findShortestPath(to)?.toList()

    fun findLeastTurningPath(from: Position) : List<Direction>? = map[from]?.findLeastTurningPath()?.toList()

    override fun toString() : String {
        val minX = map.minBy { it.key.first }!!.key.first
        val maxX = map.maxBy { it.key.first }!!.key.first
        val minY = map.minBy { it.key.second }!!.key.second
        val maxY = map.maxBy { it.key.second }!!.key.second

        var res = ""
        for (y in minY..maxY) {
            for (x in minX..maxX) {
                res += map[x to y]?.toString() ?: " "
            }
            res += "\n"
        }

        return res
    }

    private fun findNeighbors(position: Position) =
        Direction.values().map { position + it }.mapNotNull { map[it] }

    abstract class Block(val position: Position, val neighbors: MutableList<Block> = mutableListOf()) {
        var visited = false
        abstract fun walk(to: Position, prevPos: Position) : MutableList<Direction>?
        fun leastTurnWalk(prevPos: Position): MutableList<Direction>? {
            val dirHere = direction(prevPos, position)
            val wantedWalkPos = position + dirHere
            val walkables = neighbors.filterIsInstance<Path>()
            val wantedWalkPath = walkables.find { it.position == wantedWalkPos }

            var nextWalk = wantedWalkPath
            if(wantedWalkPath == null) nextWalk = walkables.find { !it.visited }

            if(nextWalk == null) return mutableListOf(dirHere)

            visited = true
            val res = nextWalk.leastTurnWalk(position)
            visited = false

            res?.add(0, dirHere)
            return res
        }

        fun findShortestPath(to: Position) : MutableList<Direction>? {
            if (to == position) return mutableListOf()

            visited = true
            val steps = neighbors.mapNotNull { it.walk(to, position) }.minBy { it.size }
            visited = false
            return steps
        }

        fun findLeastTurningPath(): MutableList<Direction>? = neighbors.find { it is Path}?.leastTurnWalk(position)
    }

    class Path(position: Position) : Block(position) {
        override fun walk(to: Position, prevPos: Position) : MutableList<Direction>? {
            if (to == position) return mutableListOf(direction(prevPos, position))

            visited = true
            val steps = neighbors
                .filter { !it.visited }
                .mapNotNull { it.walk(to, position) }
                .minBy { it.size }
            visited = false

            steps?.add(0, direction(prevPos, position))
            return steps
        }

        override fun toString(): String = "."
    }

    class Wall(position: Position) : Block(position) {
        override fun walk(to: Position, prevPos: Position) : MutableList<Direction>? = null
        override fun toString(): String = "#"
    }
}

