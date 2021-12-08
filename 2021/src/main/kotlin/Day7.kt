import kotlin.math.abs

class Day7 : Day {
    private fun parseInput(input: List<String>): List<Int> {
        return input.first().split(",").map { it.toInt() }
    }

    // 475 too low
    // 353812 too high
    override fun part1(input: List<String>): String {
        val positions = parseInput(input)
        val medianPosition  = positions.sorted()[positions.size / 2]
        val fuelConsumption = positions.sumOf { abs(it - medianPosition) }
        return fuelConsumption.toString()
    }

    override fun part2(input: List<String>): String {
        val positions = parseInput(input)
        val minPosition = positions.minOrNull()!!
        val maxPosition = positions.maxOrNull()!!
        val fuelCosts = (minPosition..maxPosition).minOf { fuelCost(positions, it) }
        return fuelCosts.toString()
    }

    private fun fuelCost(positions: List<Int>, meetupPos: Int) : Int {
        return positions.sumOf { abs(it - meetupPos).let { distance -> if(distance > 0) (1..distance).sum() else 0 } }
    }

}