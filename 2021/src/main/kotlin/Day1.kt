class Day1 : Day {
    override fun part1(input: List<String>): String {
        val depths = parseToInt(input)
        return depths.zipWithNext().filter { it.first < it.second }.size.toString()
    }

    override fun part2(input: List<String>): String {
       return ""
    }
}