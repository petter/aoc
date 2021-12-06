class Day6 : Day {
    private fun parseInput(input: List<String>): Map<Int, Long> {
        return input.first().let {
            it.split(",").map { num -> num.toInt() }
        }.groupBy { it }
            .map { it.key to it.value.size.toLong() }.toMap()
    }

    private fun simulateDay(lanternfish: Map<Int, Long>) : Map<Int, Long> {
        val result = mutableMapOf<Int, Long>()
        for (i in 0 .. 8) {
            val group = lanternfish.getOrDefault(i, 0)
            if(i == 0) {
                result[8] = group
                result[6] = group
            } else {
                val amountAtPos = result.getOrDefault(i - 1, 0)
                result[i - 1] = group + amountAtPos
            }
        }
        return result
    }

    override fun part1(input: List<String>): String {
        var lanternfish = parseInput(input)
        for (day in 0 until 80) {
            lanternfish = simulateDay(lanternfish)
        }
        return lanternfish.map { it.value }.sum().toString()
    }

    // 452544845 too low
    override fun part2(input: List<String>): String {
        var lanternfish = parseInput(input)
        for (day in 0 until 256) {
            lanternfish = simulateDay(lanternfish)
        }
        return lanternfish.map { it.value }.sum().toString()
    }
}