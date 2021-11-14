import java.io.File

fun main() {
    runLatest()
}

fun run(day: Int) {
    val daySolution = solutions[day]

    if (daySolution == null) {
        println("Day $day is not implemented yet.")
        return
    }

    val input = File("src/main/resources/input/day$day.txt").readLines()

    println("Day $day part 1: ${daySolution.part1(input)}")
    println("Day $day part 2: ${daySolution.part2(input)}")
}

fun runLatest() {
    solutions.maxByOrNull { it.key }?.let {
        run(it.key)
    }
}

val solutions = mapOf<Int, Day>(
    1 to Day1(),
)

val expectedResults = mapOf(
    1 to Pair(3273471, 4907345),
    2 to Pair(11590668, 2254),
    3 to Pair(2427, 27890),
    4 to Pair(1099, 710),
    5 to Pair(251208, 397),
    6 to Pair(0, 0),
    7 to Pair(0, 0),
    8 to Pair(0, 0),
    9 to Pair(0, 0),
    10 to Pair(0, 0),
    11 to Pair(0, 0),
    12 to Pair(0, 0),
    13 to Pair(0, 0),
    14 to Pair(0, 0),
    15 to Pair(0, 0),
    16 to Pair(0, 0),
    17 to Pair(0, 0),
    18 to Pair(0, 0),
    19 to Pair(0, 0),
    20 to Pair(0, 0),
    21 to Pair(0, 0),
    22 to Pair(0, 0),
    23 to Pair(0, 0),
    24 to Pair(0, 0),
    25 to Pair(0, 0),
)