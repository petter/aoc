package day15

import IntCodeProgram

typealias Position = Pair<Int, Int>

fun main() {
    val program = IntCodeProgram.fromFile("in/day15.txt")
    val robot = Robot(program)
    robot.explore()
    println(robot.maze.toString())
    println("Shortest path to oxygen: ${robot.maze.findShortestPath(robot.maze.startPos, robot.maze.finishPosition!!)?.size}")

    // Should have done a BFS instead, but I'm lazy
    val allPositions = robot.maze.map.filter { it.value is Maze.Path }.map { it.key }
    val longestPath = allPositions.map { robot.maze.findShortestPath(robot.maze.finishPosition!!, it)!!.size }.max()
    println("Everything will be filled with oxygen in $longestPath mins")
}

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
        to.second < from.second -> Direction.South
        else -> Direction.North
    }

infix operator fun Pair<Int, Int>.plus(direction: Direction) : Pair<Int, Int> =
    when (direction) {
        Direction.North -> this.first     to this.second + 1
        Direction.South -> this.first     to this.second - 1
        Direction.East  -> this.first + 1 to this.second
        Direction.West  -> this.first - 1 to this.second
    }

class Robot(val program: IntCodeProgram) {
    var position = Pair(0,0)
    val maze = Maze(position)

    fun explore() {
        var explorables = findExplorables()
        while (explorables.isNotEmpty()) {
            println(maze.toString())
            for (explorable in explorables) {
                walkTo(explorable.position)
                position = explorable.position
                val exploredDirections = explorable.neighbors.map { direction(position, it.position) }
                val toExplore = Direction.values().filter { !exploredDirections.contains(it) }
                toExplore.forEach(::exploreDirection)
            }
            explorables = findExplorables()
        }
    }

    private fun exploreDirection(direction: Direction) {
        program.inputChannel.add(direction.num)
        program.run()
        val res = program.outputChannel.removeAt(0).toInt()
        val walkPos = position + direction
        maze.addBlock(walkPos, res)

        if (res != 0) {
            val to = position
            position = walkPos
            walkTo(to)
        }

    }

    fun walkTo(to: Position) {
        val steps = maze.findShortestPath(position, to) ?: error("Tried moving robot to unexplored tile.")
        program.inputChannel.addAll(steps.map { it.num })
        while (program.inputChannel.isNotEmpty()) program.run()

        if(program.outputChannel.any {it == 0L}) error("Robot hit an unexpected wall, path is wrong.")
        program.outputChannel.clear()
        position = to
    }

    fun findExplorables() = maze.map.map { it.value }.filter { it is Maze.Path && it.neighbors.size < 4 }
}

class Maze(val startPos: Position) {
    var finishPosition: Position? = null
    val map = mutableMapOf<Position, Block>(startPos to Path(startPos))


    fun addBlock(position: Position, type: Int) {
        val block = when (type) {
            0 -> Wall(position)
            1 -> Path(position)
            2 -> {
                finishPosition = position
                Path(position)
            }
            else -> error("Invalid block type $type")
        }

        map[position] = block

        val neighbors = findNeighbors(position)
        neighbors.forEach { it.neighbors.add(block) }
        block.neighbors.addAll(neighbors)
    }

    fun findShortestPath(from: Position, to: Position) : List<Direction>? = map[from]?.findShortestPath(to)?.toList()

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
        fun findShortestPath(to: Position) : MutableList<Direction>? {
            if (to == position) return mutableListOf()

            visited = true
            val steps = neighbors.mapNotNull { it.walk(to, position) }.minBy { it.size }
            visited = false
            return steps
        }
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

