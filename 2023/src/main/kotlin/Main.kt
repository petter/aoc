import java.io.File

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

    val part1TimeStart = System.currentTimeMillis()
    val solutionPart1 = daySolution.part1(input)
    val part1Time = System.currentTimeMillis() - part1TimeStart

    val part2TimeStart = System.currentTimeMillis()
    val solutionPart2 = daySolution.part2(input)
    val part2Time = System.currentTimeMillis() - part2TimeStart

    if (expectedResult == null) {
        println("Could not find any expected results")

        println("Part 1: $solutionPart1 - ${part1Time}ms")
        println("Part 2: $solutionPart2 - ${part2Time}ms")
    } else {
        val part1AsExpected = solutionPart1 == expectedResult.first
        val part2AsExpected = solutionPart2 == expectedResult.second

        if (part1AsExpected && part2AsExpected) {
            println("✅ OK! Both parts are working as expected")
            println("Part 1 took ${part1Time}ms")
            println("Part 2 took ${part2Time}ms")
            return
        }

        println("❌ Not OK...")

        if (part1AsExpected) {
            println("Part 1 is OK")
            println("Part 1 took ${part1Time}ms")
        } else {
            println("\nPart 1 is wrong")
            println("Expected results: ${expectedResult.first}")
            println("Actual results: $solutionPart1")
            println("Part 1 took ${part1Time}ms")
        }

        if (part2AsExpected) {
            println("Part 2 is OK")
            println("Part 2 took ${part2Time}ms")
        } else {
            println("\nPart 2 is wrong")
            println("Expected results: ${expectedResult.second}")
            println("Actual results: $solutionPart2")
            println("Part 2 took ${part2Time}ms")
        }
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
//    2 to Day2(),
//    3 to Day3(),
//    4 to Day4(),
//    5 to Day5(),
//    6 to Day6(),
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
    1 to Pair("0", "0"),
//    2 to Pair("0", "0"),
//    3 to Pair("0", "0"),
//    4 to Pair("0", "0"),
//    5 to Pair("0", "0"),
//    6 to Pair("0", "0"),
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
