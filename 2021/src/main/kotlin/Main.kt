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

    val solutionPart1 = daySolution.part1(input)
    val solutionPart2 = daySolution.part2(input)
    if (expectedResult == null) {
        println("Could not find any expected results")

        println("Part 1: $solutionPart1")
        println("Part 2: $solutionPart2")
    } else {
        val part1AsExpected = solutionPart1 == expectedResult.first
        val part2AsExpected = solutionPart2 == expectedResult.second

        if (part1AsExpected && part2AsExpected) {
            println("✅ OK! Both parts are working as expected")
            return
        }

        println("❌ Not OK...")

        if (part1AsExpected) {
            println("Part 1 is OK")
        } else {
            println("\nPart 1 is wrong")
            println("Expected results: ${expectedResult.first}")
            println("Actual results: $solutionPart1")
        }

        if (part2AsExpected) {
            println("Part 2 is OK")
        } else {
            println("\nPart 2 is wrong")
            println("Expected results: ${expectedResult.second}")
            println("Actual results: $solutionPart2")
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
    2 to Day2(),
    3 to Day3(),
    4 to Day4(),
    5 to Day5(),
)

val expectedResults = mapOf<Int, Pair<String, String>>(
    1 to Pair("1446", "1486"),
    2 to Pair("1989014", "2006917119"),
    3 to Pair("4191876", "3414905"),
    4 to Pair("63424", "23541"),
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
