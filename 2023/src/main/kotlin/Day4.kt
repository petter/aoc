import kotlin.math.pow

class Day4 : Day {
    private fun parseInput(input: List<String>): List<Scratchcard> {
        return input.map { line ->
            val id = line
                .substringAfter("Card ")
                .substringBefore(":")
                .trim()
                .toInt()
            val winningNumbers = line
                .substringAfter(": ")
                .substringBefore(" |")
                .split(" ")
                .filter { it.trim().isNotBlank() }
                .map { it.trim().toInt() }
            val numbers = line
                .substringAfter("| ")
                .split(" ")
                .filter { it.trim().isNotBlank() }
                .map { it.trim().toInt() }
            Scratchcard(id, numbers, winningNumbers)
        }
    }

    override fun part1(input: List<String>): String {
        val scratchcards = parseInput(input)
        return scratchcards.sumOf { it.getPoints() }.toString()
    }

    override fun part2(input: List<String>): String {
        val scratchcards = parseInput(input)
        // Map of id to (scratchcard, amount)
        val scratchcardMap = scratchcards.associate { it.id to (it to 1) }.toMutableMap()

        for (key in scratchcardMap.keys) {
            val (curScratchcard, amount) = scratchcardMap.getOrDefault(key, Pair(null, 0))
            if(curScratchcard != null) {
                val winners = curScratchcard.getWinners()
                for(i in (key + 1)..(key + winners)) {
                    val (otherScratchcard, otherAmount) = scratchcardMap.getOrDefault(i, Pair(null, 0))
                    if(otherScratchcard != null) {
                        scratchcardMap[i] = Pair(otherScratchcard, otherAmount + amount)
                    }
                }
            }
        }

        return scratchcardMap.values.sumOf { it.second }.toString()
    }

}

private data class Scratchcard(val id: Int, val numbers: List<Int>, val winningNumbers: List<Int>) {
    fun getPoints(): Int {
        val winners = getWinners()
        if (winners == 0) {
            return 0
        }

        return 2.0.pow(winners - 1).toInt()
    }

    // How many numbers are winners
    fun getWinners(): Int {
        return numbers.filter { it in winningNumbers }.size
    }
}