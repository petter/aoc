class Day11 : Day {
    private fun parseInput(input: List<String>) : Map<Pair<Int, Int>, Int> {
        return input
            .flatMapIndexed { y, line ->
                line
                    .toCharArray()
                    .mapIndexed { x, num -> Pair(x, y) to num.toString().toInt() }
            }.toMap()
    }

    override fun part1(input: List<String>): String {
        val octopuses = parseInput(input).toMutableMap()
        var flashes = 0
        for (step in 1..100) {
            flashes += simulateStep(octopuses)
        }
        return flashes.toString()
    }

    private fun simulateStep(octopuses: MutableMap<Pair<Int, Int>, Int>) : Int {
        incrementPoses(octopuses, octopuses.keys.toList())
        val flashes = mutableSetOf<Pair<Int, Int>>()

        do {
            val newFlashes = octopuses.filter { it.value > 9 }.keys.filter { it !in flashes }
            flashes.addAll(newFlashes)
            val newFlashNeighbors = newFlashes.flatMap { getNeighbors(octopuses, it) }
            incrementPoses(octopuses, newFlashNeighbors)
        } while (newFlashes.isNotEmpty())

        flashes.forEach { octopuses[it] = 0 }

        return flashes.size
    }

    private fun incrementPoses(octopuses: MutableMap<Pair<Int, Int>, Int>, poses: List<Pair<Int, Int>>) {
        poses.forEach { octopuses.computeIfPresent(it) { _, value -> value + 1 } }
    }

    private fun getNeighbors(octopuses: Map<Pair<Int, Int>, Int>, pos: Pair<Int, Int>) : Set<Pair<Int, Int>> {
        val (x, y) = pos
        return listOf(
            Pair(x - 1, y),
            Pair(x + 1, y),
            Pair(x, y - 1),
            Pair(x, y + 1),
            Pair(x - 1, y - 1),
            Pair(x + 1, y + 1),
            Pair(x - 1, y + 1),
            Pair(x + 1, y - 1)
        ).filter { it in octopuses }.toSet()
    }

    override fun part2(input: List<String>): String {
        val octopuses = parseInput(input).toMutableMap()
        var step = 0
        do {
            step += 1
            val flashes = simulateStep(octopuses)
        }
        while (flashes != octopuses.size)
        return step.toString()
    }
}