package day17

import IntCodeProgram
import java.util.zip.Deflater
import kotlin.math.min

typealias Position = Pair<Int, Int>

fun main() {
    solveA()
    solveB()
}

fun createIntcodeProgram(): IntCodeProgram {

    val program = IntCodeProgram.fromFile("in/day17.txt")
    return program
}
fun createMaze(): Maze {
    val program = createIntcodeProgram()
    val maze = Maze()

    while(program.run() != IntCodeProgram.RunState.DONE);

    val inputMaze = String(program.outputChannel.map { it.toChar() }.toCharArray()).lines()
    inputMaze.forEachIndexed { y, line -> line.forEachIndexed { x, c ->
        maze.addBlock(x to y, if(c == '.') 0 else 1)
        if(c == '^') maze.robotPosition = x to y
    } }
    return maze
}

fun solveA() {
    val maze = createMaze()
    val alignmentParameterSum = maze.map.filter { it.value is Maze.Path && it.value.neighbors.filterIsInstance<Maze.Path>().size == 4 }.map { it.value.position.first * it.value.position.second }.sum()
    println("Sum of alignment parameter sum: $alignmentParameterSum")
}

fun solveB() {
    val program = createIntcodeProgram()
    program.changeMemory(0, 2L)
    val maze = createMaze()
    val steps = maze.findLeastTurningPath(maze.robotPosition!!)!!

    var curDir = Direction.North
    val reducedSteps = mutableListOf(turn(curDir, steps.first()) to 0)
    curDir = steps.first()
    for (step in steps) {
        val curPair = reducedSteps.last()
        if (step == curDir) {
            reducedSteps[reducedSteps.size - 1] = curPair.first to curPair.second + 1
        } else {
            reducedSteps.add(turn(curDir, step) to 1)
            curDir = step
        }
    }

    val stepString = reducedSteps.joinToString (",", transform = { "${it.first},${it.second}" })
    //val patterns = mutableListOf<String>()
    //var tempString = stepString
    //for (i in 1..3) {
    //    val pattern = findPattern(tempString)
    //    patterns.add(pattern)
    //    tempString = tempString.replace("$pattern,", "")
    //}

    val patterns = listOf("L,4,R,8,L,6,L,10", "L,6,R,8,R,10,L,6,L,6", "L,4,L,4,L,10")
    val finalString = patterns.foldIndexed(stepString, {i, acc, pattern ->  acc.replace(pattern, (i+65).toChar().toString())})
    println(patterns)
    println("Instruction: $finalString")

    stringToInput(program, finalString)
    patterns.forEach { stringToInput(program, it) }
    stringToInput(program, "n")
    println(program.inputChannel)

    while (program.run() != IntCodeProgram.RunState.DONE);

    println(program.outputChannel.last())
}

fun stringToInput(program: IntCodeProgram, s: String) {
    s.forEach { program.inputChannel.add(it.toLong()) }
    program.inputChannel.add(10L)
}

fun findPattern(s : String) : String {
    var prevMatches = 0

    var pattern = ""
    for (i in 4 until min(s.length, 21)) {
        if(s[i] == ',') continue
        val newPattern = s.substring(0..i)
        val numMatches = Regex(newPattern).findAll(s).count()
        if(numMatches < prevMatches) return pattern
        prevMatches = numMatches
        pattern = newPattern
    }
    return pattern
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
        to.second < from.second -> Direction.North
        else -> Direction.South
    }

fun turn(from: Direction, to: Direction) : String {
   val orderedDirections = listOf(Direction.North, Direction.East, Direction.South, Direction.West)
    val fromIndex = orderedDirections.indexOf(from)
    return when (orderedDirections.indexOf(to)) {
        (fromIndex + 1) % orderedDirections.size ->  "R"
        (fromIndex + 3) % orderedDirections.size ->  "L"
        else                                     ->  "R,R"
    }
}

infix operator fun Pair<Int, Int>.plus(direction: Direction) : Pair<Int, Int> =
    when (direction) {
        Direction.North -> this.first     to this.second - 1
        Direction.South -> this.first     to this.second + 1
        Direction.East  -> this.first + 1 to this.second
        Direction.West  -> this.first - 1 to this.second
    }

//class Robot(val program: IntCodeProgram) {
//    var position = Pair(0,0)
//    val maze = Maze(position)
//
//    fun explore() {
//        var explorables = findExplorables()
//        while (explorables.isNotEmpty()) {
//            println(maze.toString())
//            for (explorable in explorables) {
//                walkTo(explorable.position)
//                position = explorable.position
//                val exploredDirections = explorable.neighbors.map { direction(position, it.position) }
//                val toExplore = Direction.values().filter { !exploredDirections.contains(it) }
//                toExplore.forEach(::exploreDirection)
//            }
//            explorables = findExplorables()
//        }
//    }
//
//    private fun exploreDirection(direction: Direction) {
//        program.inputChannel.add(direction.num)
//        program.run()
//        val res = program.outputChannel.removeAt(0).toInt()
//        val walkPos = position + direction
//        maze.addBlock(walkPos, res)
//
//        if (res != 0) {
//            val to = position
//            position = walkPos
//            walkTo(to)
//        }
//
//    }
//
//    fun walkTo(to: Position) {
//        val steps = maze.findShortestPath(position, to) ?: error("Tried moving robot to unexplored tile.")
//        program.inputChannel.addAll(steps.map { it.num })
//        while (program.inputChannel.isNotEmpty()) program.run()
//
//        if(program.outputChannel.any {it == 0L}) error("Robot hit an unexpected wall, path is wrong.")
//        program.outputChannel.clear()
//        position = to
//    }
//
//    fun findExplorables() = maze.map.map { it.value }.filter { it is Maze.Path && it.neighbors.size < 4 }
//}

class Maze() {
    var robotPosition : Position? = null
    val map = mutableMapOf<Position, Block>()

    fun addBlock(position: Position, type: Int) {
        val block = when (type) {
            0 -> Wall(position)
            1 -> Path(position)
            2 -> Path(position)
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
                res += if(Pair(x, y) == robotPosition) '^' else map[x to y]?.toString() ?: " "
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

