import java.io.File
import java.text.DecimalFormat
import kotlin.math.roundToLong
import kotlin.system.measureNanoTime

fun main(args: Array<String>) {
    if(args.size != 1) {
        runLatest()
    } else if(args.first() == "all") {
        runAll()
    } else {
       run(args.first().toInt())
    }
}

fun run(day: Int) {
    println("\n======== Day $day ========")

    val daySolution = solutions[day]

    if (daySolution == null) {
        println("Day $day is not implemented yet.")
        return
    }

    val input = File("src/main/resources/input/day$day.txt").readLines()
    val expectedResult = expectedResults[day]

    val (solutionPart1, part1Time) = timeIt { daySolution.part1(input) }
    val (solutionPart2, part2Time) = timeIt { daySolution.part2(input) }

    if (expectedResult == null) {
        println("Could not find any expected results")

        println("Part 1: $solutionPart1 - ${part1Time}ms")
        println("Part 2: $solutionPart2 - ${part2Time}ms")
    } else {
        val part1AsExpected = solutionPart1 == expectedResult.first
        val part2AsExpected = solutionPart2 == expectedResult.second

        if (part1AsExpected && part2AsExpected) {
            println("✅ OK! Both parts are working as expected")
            println("Part 1 took $part1Time")
            println("Part 2 took $part2Time")
            return
        }

        println("❌ Not OK...")

        if (part1AsExpected) {
            println("Part 1 is OK")
            println("Part 1 took $part1Time")
        } else {
            println("\nPart 1 is wrong")
            println("Expected results: ${expectedResult.first}")
            println("Actual results: $solutionPart1")
            println("Part 1 took $part1Time")
        }

        if (part2AsExpected) {
            println("Part 2 is OK")
            println("Part 2 took $part2Time")
        } else {
            println("\nPart 2 is wrong")
            println("Expected results: ${expectedResult.second}")
            println("Actual results: $solutionPart2")
            println("Part 2 took $part2Time")
        }
    }

}

/**
 * Returns the time it took to run the function as a formatted string.
 * If the time is less than 1 second, it will do a more scientific timing, where we run it several times,
 * to account for JIT compilation, and other performance impacting factors.
 */
private fun <T> timeIt(f: () -> T) : Pair<T, String> {
    var res: T
    val initialTotalTime = measureNanoTime { res = f() }

    // Longer than 2sec
    if(initialTotalTime > 2e9) {
        return res to formatTime(initialTotalTime)
    }

    val times = (0..10).map { measureNanoTime { f() } }
    val averageTime = times.average().roundToLong()
    val deltaTime = averageTime - times.min()
    return res to "${formatTime(averageTime)} +- ${formatTime(deltaTime)}"
}

private fun formatTime(timeNs: Long) : String {
    val df = DecimalFormat("#.##")
    return if(timeNs >= 1e9) {
        "${df.format(timeNs.toDouble() / 1e9)}s"
    } else if(timeNs >= 1e5) {
        "${df.format(timeNs.toDouble() / 1e6)}ms"
    } else if(timeNs >= 1e2) {
        "${df.format(timeNs.toDouble() / 1e3)}μs"
    } else {
        "${timeNs}ns"
    }
}


fun runLatest() {
    solutions.maxByOrNull { it.key }?.let {
        run(it.key)
    }
}

fun runAll() {
    solutions.forEach {
        run(it.key)
    }
}

val solutions = mapOf(
    1 to Day1(),
    2 to Day2(),
    3 to Day3(),
    4 to Day4(),
    5 to Day5(),
    6 to Day6(),
//    7 to Day7(),
//    8 to Day8(),
//    9 to Day9(),
//    10 to Day10(),
//    11 to Day11(),
//    12 to Day12(),
//    13 to Day13(),
//    14 to Day14(),
//    15 to Day15(),
//    16 to Day16()
//    17 to Day17()
//    18 to Day18()
//    19 to Day19()
//    20 to Day20()
//    21 to Day21()
//    22 to Day22()
//    23 to Day23()
//    24 to Day24()
//    25 to Day25()
)

val expectedResults = mapOf(
    1 to Pair("55090", "54845"),
    2 to Pair("2105", "72422"),
    3 to Pair("527369", "73074886"),
    4 to Pair("21821", "5539496"),
    5 to Pair("462648396", "2520479"),
    6 to Pair("2449062", "33149631"),
//    7 to Pair("0", "0"),
//    8 to Pair("0", "0"),
//    9 to Pair("0", "0"),
//    10 to Pair("0", "0"),
//    11 to Pair("0", "0"),
//    12 to Pair("0", "0"),
//    13 to Pair("0", "0"),
//    14 to Pair("0", "0"),
//    15 to Pair("0", "0"),
//    16 to Pair("0", "0"),
//    17 to Pair("0", "0"),
//    18 to Pair("0", "0"),
//    19 to Pair("0", "0"),
//    20 to Pair("0", "0"),
//    21 to Pair("0", "0"),
//    22 to Pair("0", "0"),
//    23 to Pair("0", "0"),
//    24 to Pair("0", "0"),
//    25 to Pair("0", "0"),
)
