class Day8 : Day {
    override fun part1(input: List<String>): String {
        return input.sumOf { line ->
            val output = line.split(" | ")[1]
            output.split(" ").filter { it.length in listOf(2, 3, 4, 7) }.size
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return ""
    }
}