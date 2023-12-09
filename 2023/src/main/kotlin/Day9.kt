class Day9 : Day {

    private fun parseInput(input: List<String>): List<List<Int>> {
        return input.map { line -> line.split(" ").map { it.toInt() } }
    }


    private fun getNextHistoryNumber(input: List<Int>): Int {
        val sequence = getSequences(input)
        var prevHistoryNumber = 0
        for(i in (sequence.size - 1) downTo 0) {
            val thisLineHistoryNumber = sequence[i].last() + prevHistoryNumber
            prevHistoryNumber = thisLineHistoryNumber
        }

        return prevHistoryNumber
    }

    private fun getSequences(numbers: List<Int>): List<List<Int>> {
        val sequences = mutableListOf(numbers)

        var currentSequence = numbers
        while (currentSequence.any { it != 0 }) {
            currentSequence = currentSequence.zipWithNext().map { (a, b) -> b - a }
            sequences.add(currentSequence)
        }

        return sequences
    }

    override fun part1(input: List<String>): String {
        val numbers = parseInput(input)
        return numbers.sumOf { getNextHistoryNumber(it) }.toString()
    }

    private fun getPrevHistoryNumber(input: List<Int>): Int {
        val sequence = getSequences(input)
        var prevHistoryNumber = 0
        for(i in (sequence.size - 1) downTo 0) {
            val thisLineHistoryNumber = sequence[i].first() - prevHistoryNumber
            prevHistoryNumber = thisLineHistoryNumber
        }

        return prevHistoryNumber
    }

    // 21228 - too high
    override fun part2(input: List<String>): String {
        val numbers = parseInput(input)
        return numbers.sumOf { getPrevHistoryNumber(it) }.toString()
    }

}