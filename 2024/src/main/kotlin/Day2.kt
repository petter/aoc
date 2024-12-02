import kotlin.math.abs

class Day2 : Day {

    private fun parseInput(input: List<String>): List<List<Int>> {
        return input.map { it.split(" ").map { num -> num.toInt() } }
    }

    override fun part1(input: List<String>): String {
        val reports = parseInput(input)
        val safeReports = reports.filter { isSafe(it) }
        return safeReports.size.toString()
    }

    private fun isSafe(report: List<Int>): Boolean {
        val increasingOrDecreaseing = isIncreasing(report) || isDecreasing(report)
        val smallAdjacents = isSmallAdjacents(report)

        return increasingOrDecreaseing && smallAdjacents
    }

    private fun isIncreasing(report: List<Int>) = report.zipWithNext().all { (a, b) -> a < b }
    private fun isDecreasing(report: List<Int>) = report.zipWithNext().all { (a, b) -> a > b }

    private fun isSmallAdjacents(report: List<Int>) = report.zipWithNext().all { (a, b) -> abs(a - b) in 1..3 }

    override fun part2(input: List<String>): String {
        val reports = parseInput(input)
        return reports.count { trySafe(it) }.toString()
    }

    private fun trySafe(report: List<Int>): Boolean {
        if(isSafe(report)) {
            return true
        }

        val reportsWithOneLess = report.indices.map { i -> report.toMutableList().apply { removeAt(i) } }
        return reportsWithOneLess.any { isSafe(it) }
    }

}