import java.io.File

fun main() {
    runLatest()
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
            println("OK! Both parts are working as expected")
            return
        }

        println("Not OK...")

        if (part1AsExpected) {
            println("Part 1 is OK")
        } else {
            println("Part 1 is wrong")
            println("Expected results: ${expectedResult.first}")
            println("Actual results: $solutionPart1")
        }

        if (part2AsExpected) {
            println("Part 2 is OK")
        } else {
            println("Part 2 is wrong")
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

val solutions = mapOf<Int, Day>(
    1 to Day1(),
)

val expectedResults = mapOf(
    1 to Pair("3273471", "4907345"),
    2 to Pair("11590668", "2254"),
    3 to Pair("2427", "27890"),
    4 to Pair("1099", "710"),
    5 to Pair("15508323", "9006327"),
    6 to Pair("251208", "397"),
    7 to Pair("87138", "17279674"),
    8 to Pair("1320", "RCYKR"),
    9 to Pair("2494485073", "44997"),
    10 to Pair("247", "1919"),
    11 to Pair("2226", "HBGLZKLF"),
    12 to Pair("6490", "277068010964808"),
    13 to Pair("341", "17138"),
    14 to Pair("899155", "2390226"),
    15 to Pair("218", "544"),
//    16 to Pair("0", "0"),
    17 to Pair("3192", "684691"),
//    18 to Pair("0", "0"),
//    19 to Pair("0", "0"),
//    20 to Pair("0", "0"),
//    21 to Pair("0", "0"),
//    22 to Pair("0", "0"),
//    23 to Pair("0", "0"),
//    24 to Pair("0", "0"),
//    25 to Pair("0", "0"),
)