class Day3 : Day {
    private fun parseInput(input: List<String>): List<Pair<Int, Int>> {
        val s = input.joinToString { it }
        val matcher = Regex("mul\\((\\d+),(\\d+)\\)")
        val matches = matcher.findAll(s)
        return matches.map { it.groupValues.drop(1).map { value -> value.toInt() }.toPair() }.toList()
    }

    override fun part1(input: List<String>): String {
        val multiplications = parseInput(input)
        return multiplications.sumOf { (a, b) -> a * b }.toString()
    }

    override fun part2(input: List<String>): String {
        return ""
    }

}