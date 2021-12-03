class Day3 : Day {
    override fun part1(input: List<String>): String {
        val gammaRate = mutableListOf<Char>()
        for (pos in 0 until input[0].length) {
            val mostCommon = mostCommonBitAtPos(input, pos)
            gammaRate.add(mostCommon)
        }

        val gammaString = gammaRate.joinToString("")
        val gamma = binaryStringToInt(gammaString)
        val epsilon = binaryStringToInt(inverted(gammaString))

        return (gamma * epsilon).toString()
    }

    override fun part2(input: List<String>): String {
        var oxygenCandidates = input
        var co2Candidates = input
        for (i in 0 until input[0].length) {
            val oxygenCriteria = oxygenBitCriteria(oxygenCandidates, i)
            val co2Criteria = co2BitCriteria(co2Candidates, i)
            if (oxygenCandidates.size > 1) {
                oxygenCandidates = oxygenCandidates.filter { it[i] == oxygenCriteria }
            }
            if (co2Candidates.size > 1) {
                co2Candidates = co2Candidates.filter { it[i] == co2Criteria }
            }
        }

        val oxygenGeneratorRating = binaryStringToInt(oxygenCandidates.first())
        val co2ScrubberRating = binaryStringToInt(co2Candidates.first())

        return (oxygenGeneratorRating * co2ScrubberRating).toString()
    }

    fun groupedByBitAtPos(input: List<String>, pos: Int): Map<Char, List<Char>> {
        val posBits = mutableListOf<Char>()
        for (line in input) {
            posBits.add(line[pos])
        }
        return posBits.groupBy { it }
    }

    fun mostCommonBitAtPos(input: List<String>, pos: Int): Char {
        val grouped = groupedByBitAtPos(input, pos)
        return grouped.maxByOrNull { it.value.size }!!.key
    }

    fun oxygenBitCriteria(input: List<String>, pos: Int): Char {
        val grouped = groupedByBitAtPos(input, pos)
        val num1s = grouped['1']?.size ?: 0
        val num0s = grouped['0']?.size ?: 0
        if(num1s == num0s) {
             return '1';
        }
        return grouped.maxByOrNull { it.value.size }!!.key
    }

    fun co2BitCriteria(input: List<String>, pos: Int): Char {
        val grouped = groupedByBitAtPos(input, pos)
        val num1s = grouped['1']?.size ?: 0
        val num0s = grouped['0']?.size ?: 0
        if(num1s == num0s) {
            return '0';
        }
        return grouped.minByOrNull { it.value.size }!!.key
    }

    fun inverted(input: String): String {
        val inverted = mapOf('0' to '1', '1' to '0')
        return input.map { inverted[it]!! }.joinToString("")
    }

    fun binaryStringToInt(binaryString: String): Int {
        return binaryString.map { it.toString().toInt() }.reduce { acc, i -> acc * 2 + i }
    }
}