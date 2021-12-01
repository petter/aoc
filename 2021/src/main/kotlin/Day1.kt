class Day1 : Day {

    fun increasingDepths(depthPairs: List<Pair<Int, Int>>) = depthPairs.filter { it.first < it.second }.size

    override fun part1(input: List<String>): String {
        val depths = parseToInt(input)
        val depthPairs = depths.zipWithNext()
        return increasingDepths(depthPairs).toString()
    }

    override fun part2(input: List<String>): String {
        val depths = parseToInt(input)
        val depthPairs = depths
            .zipWithNext()
            .zipWithNext()
            .map { it.first.first + it.second.first + it.second.second }
            .zipWithNext()
        return increasingDepths(depthPairs).toString()
    }
}