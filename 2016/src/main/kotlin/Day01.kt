import Direction.*
import kotlin.math.abs

enum class Direction {
    NORTH, SOUTH, WEST, EAST
}

private operator fun Pair<Int, Int>.plus(other : Pair<Int, Int>) = Pair(this.first + other.first, this.second + other.second)

class Walker(var pos : Pair<Int, Int> = Pair(0, 0)) {
    var facing : Direction = NORTH;
    val visitedLocations : MutableList<Pair<Int, Int>> = mutableListOf(Pair(0, 0))

    fun walk(command: String) {
        val facing = command[0]
        val length = command.substring(1).toInt()
        changeFacing(facing)
        move(length)
    }

    fun walk2(command: String) {
        val facing = command[0]
        val length = command.substring(1).toInt()
        changeFacing(facing)
        for (i in 1..length) {
            move(1)
            visitedLocations.add(pos)
        }

    }

    private fun changeFacing(c: Char) {
        when (c) {
            'L' -> turnLeft()
            'R' -> turnRight()
            else -> error("Invalid facing argument")
        }
    }

    private fun turnLeft() {
        facing = when(facing) {
            NORTH -> WEST
            WEST -> SOUTH
            SOUTH -> EAST
            EAST -> NORTH
        }
    }

    private fun turnRight() {
        facing = when(facing) {
            NORTH -> EAST
            EAST -> SOUTH
            SOUTH -> WEST
            WEST -> NORTH
        }
    }

    private fun move(length: Int) {
        pos = when (facing) {
            NORTH -> pos + Pair(0, length)
            SOUTH -> pos + Pair(0, -length)
            WEST -> pos + Pair(-length, 0)
            EAST -> pos + Pair(length, 0)
        }
    }
}

class Day01 {
    private val input: String = javaClass.getResource("data/day1.txt").readText()

    fun solve() {
        val moves = input.trim().split(", ")
        part1(moves)
        part2(moves)
    }

    fun part1(moves: List<String>) {
        println("\n--------PART 1--------")
        val walker = Walker()
        moves.forEach(walker::walk)
        println("Position: ${walker.pos}, facing: ${walker.facing}")
        println("Answer is ${abs(walker.pos.first) + abs(walker.pos.second)}")
    }

    fun part2(moves: List<String>) {
        println("\n--------PART 2--------")
        val walker = Walker()
        moves.forEach(walker::walk2)

        walker.visitedLocations.groupBy { it }.forEach { (pos, list) ->
            if (list.size > 1) {
                println("Position: ${pos}")
                println("Answer is ${abs(pos.first) + abs(pos.second)}")
            }
        }
    }
}

fun main() {
    Day01().solve()
}