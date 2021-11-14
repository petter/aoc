class Day3 : Day {
    override fun part1(input: List<String>): String {
        val visitedPositions = input.map { line ->
            val wireMap = WireMap()
            val moves = line.split(",")
            wireMap.doMoves(moves)
            wireMap.visitedPositions.toSet()
        }

        val intersections = visitedPositions
            .reduce { acc, positions -> acc.intersect(positions) }
            .filter { it != Pair(0, 0) }
            .map { manhattanDistance(it, Pair(0, 0)) }
        val closest = intersections.minOrNull()
        return closest.toString()
    }

    override fun part2(input: List<String>): String {
        val wireMaps = input.map { line ->
            val wireMap = WireMap()
            val moves = line.split(",")
            wireMap.doMoves(moves)
            wireMap
        }
        val visitedPositions = wireMaps.map { it.visitedPositions.toSet() }
        val intersections = visitedPositions
            .reduce { acc, positions -> acc.intersect(positions) }
            .filter { it != Pair(0, 0) }
        val stepCounts = intersections.map { intersection ->
            wireMaps.sumOf { it.positionStepCount[intersection]!! }
        }
        return stepCounts.minOrNull().toString()
    }
}

private class WireMap {
    private var position = Pair(0, 0)
    val visitedPositions = mutableSetOf(position)
    val positionStepCount = mutableMapOf<Pair<Int, Int>, Int>()
    var stepCount = 0

    fun doMoves(moves: List<String>) {
        moves.forEach(this::move)
    }

    fun move(move: String) {
        val direction = move.first()
        val distance = move.substring(1).toInt()
        for (i in 1..distance) {
            val nextPos = nextPos(direction)
            position = nextPos
            visitedPositions.add(nextPos)
            positionStepCount.putIfAbsent(nextPos, ++stepCount)
        }
    }

    private fun nextPos(direction: Char): Pair<Int, Int> {
        return when(direction) {
            'U' -> Pair(position.first, position.second + 1)
            'D' -> Pair(position.first, position.second - 1)
            'L' -> Pair(position.first - 1, position.second)
            'R' -> Pair(position.first + 1, position.second)
            else -> throw Error("Invalid direction")
        }
    }
}