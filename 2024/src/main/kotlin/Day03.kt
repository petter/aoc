class Day03 : Day {
    private fun parseInput(input: List<String>, isPart2: Boolean): List<Pair<Int, Int>> {
        val s = input.joinToString { it }
        val matcher = Regex("(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))")
        val matches = matcher.findAll(s)

        var mulEnabled = true
        return matches.mapNotNull { group ->
            if (group.groupValues.first() == "do()") {
                mulEnabled = true
                return@mapNotNull null
            }
            if (group.groupValues.first() == "don't()") {
                mulEnabled = false
                return@mapNotNull null
            }
            if(!mulEnabled && isPart2) {
                return@mapNotNull null
            }
            group.groupValues.drop(2).map { value ->
                value.toInt()
            }.toPair()
        }.toList()
    }

    override fun part1(input: List<String>): String {
        val multiplications = parseInput(input, false)
        return multiplications.sumOf { (a, b) -> a * b }.toString()
    }

    // 154365 - too low
    override fun part2(input: List<String>): String {
        val multiplications = parseInput(input, true)
        return multiplications.sumOf { (a, b) -> a * b }.toString()
    }

}