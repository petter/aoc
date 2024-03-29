class Day1 : Day {

    private fun findCalibrationValues(input: List<String>): List<Int> {
        val numbers = input
            .map {
                it.toCharArray()
                .filter { char -> char.isDigit() }
            }
        return numbers
            .filter { it.isNotEmpty() }
            .map { it.first().toString() + it.last().toString() }
            .map { it.toInt() }
    }
    override fun part1(input: List<String>): String {
        val calibrationValues = findCalibrationValues(input)
        val result = calibrationValues.sum()
        return result.toString()
    }

    // Includes first and last char of each number to fix issues with overlapping numbers
    private val numberStringToNumber = mapOf(
        "one" to "o1e",
        "two" to "t2o",
        "three" to "t3e",
        "four" to "f4r",
        "five" to "f5e",
        "six" to "s6x",
        "seven" to "s7n",
        "eight" to "e8t",
        "nine" to "n9e",
    )

    override fun part2(input: List<String>): String {
        val spelledOutReplaced = input.map {
            numberStringToNumber.entries.fold(it) { acc, (key, value) ->
                acc.replace(key, value)
            }
        }
        val calibrationValues = findCalibrationValues(spelledOutReplaced)
        val result = calibrationValues.sum()
        return result.toString()
    }
}