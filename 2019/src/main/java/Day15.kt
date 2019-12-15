package day15

import java.io.File

typealias Position = Pair<Int, Int>

fun main() {
    val opcodes = File("in/day15.txt").readText().trim().split(",").map { it.toLong() }.toInfiniteArrayList()
    val program = IntCodeProgram(opcodes = opcodes)
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

class IntCodeProgram(private val opcodes: InfiniteArrayList, val inputChannel: MutableList<Long> = mutableListOf(), val outputChannel: MutableList<Long> = mutableListOf()) {
    private var ip = 0
    private var relativeBase = 0

    fun run() : RunState {
        while (opcodes[ip] != 99L) {
            val opcodeString = opcodes[ip].toString().padStart(5, '0')
            val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toString().toInt()] }.reversed()
            when (opcodeString.substring(3).toInt()) {
                1 -> doOp(Long::plus, parameterModes)
                2 -> doOp(Long::times, parameterModes)
                3 -> opcode3(parameterModes)
                4 -> { opcode4(parameterModes); return RunState.YIELDED }
                5 -> jumpIf({ it != 0L }, parameterModes)
                6 -> jumpIf({ it == 0L }, parameterModes)
                7 -> comparison({a, b -> a < b}, parameterModes)
                8 -> comparison({a, b -> a == b}, parameterModes)
                9 -> opcode9(parameterModes)
                else -> error("Unsupported opcode")
            }
        }

        return RunState.DONE
    }

    private fun doOp(op: (Long, Long) -> Long, parameterModes: List<ParameterMode>) {
        val res = getParameterValues(ip+1..ip+3, parameterModes.subList(0, 2)).reduce(op)
        write(res, ip + 3, parameterModes.last())
        ip += 4
    }

    private fun opcode3(parameterModes: List<ParameterMode>) {
        val input = inputChannel.removeAt(0)
        write(input, ip + 1, parameterModes[0])
        ip += 2
    }

    private fun opcode4(parameterModes: List<ParameterMode>) {
        val outputVal = getParameterValue(ip + 1, parameterModes[0])
        outputChannel.add(outputVal)
        ip += 2
    }

    private fun jumpIf(test: (Long) -> Boolean, parameterModes: List<ParameterMode>) {
        val (num, jumpPos) = getParameterValues(ip+1..ip+2, parameterModes.subList(0, 2))
        if(test(num)) ip = jumpPos.toInt()
        else ip += 3
    }

    private fun comparison(comp: (Long, Long) -> Boolean, parameterModes: List<ParameterMode>) {
        val (a, b) = getParameterValues(ip+1..ip+2, parameterModes.subList(0, 2))
        val res = if(comp(a, b)) 1L else 0L
        write(res, ip + 3, parameterModes.last())
        ip += 4
    }

    private fun opcode9(parameterModes: List<ParameterMode>) {
        relativeBase += getParameterValue(ip + 1, parameterModes[0]).toInt()
        ip += 2
    }

    private fun getParameterValue(ip: Int, parameterMode: ParameterMode) =
        when (parameterMode) {
            ParameterMode.POSITION_MODE  -> opcodes[opcodes[ip].toInt()]
            ParameterMode.IMMEDIATE_MODE -> opcodes[ip]
            ParameterMode.RELATIVE_MODE  -> opcodes[opcodes[ip].toInt() + relativeBase]
        }

    private fun getParameterValues(ipRange: IntRange, parameterModes: List<ParameterMode>) =
        ipRange.zip(parameterModes).map { (ip, parameterMode) -> getParameterValue(ip, parameterMode) }

    private fun write(value: Long, ip: Int, parameterMode: ParameterMode) = when (parameterMode) {
        ParameterMode.POSITION_MODE  -> opcodes[opcodes[ip].toInt()] = value
        ParameterMode.RELATIVE_MODE  -> opcodes[opcodes[ip].toInt() + relativeBase] = value
        ParameterMode.IMMEDIATE_MODE -> error("Unsupported parameter mode")
    }

    private enum class ParameterMode {
        POSITION_MODE,
        IMMEDIATE_MODE,
        RELATIVE_MODE
    }

    enum class RunState {
        YIELDED,
        DONE
    }
}

class InfiniteArrayList(): ArrayList<Long>() {
    constructor(initialList: List<Long>) : this() {
        addAll(initialList)
    }

    private fun extend(index: Int) {
        for (i in 0..(index-size)) {
            super.add(0L)
        }
    }

    override fun get(index: Int): Long {
        if(index >= size) extend(index)
        return super.get(index)
    }

    override fun set(index: Int, element: Long): Long {
        if(index >= size) extend(index)
        return super.set(index, element)
    }

}

fun List<Long>.toInfiniteArrayList() : InfiniteArrayList {
    return InfiniteArrayList(this)
}
