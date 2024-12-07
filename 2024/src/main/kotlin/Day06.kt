class Day06 : Day {
    private fun parseInput(input: List<String>): Pair<LabMap, Guard> {
        var guardPos = Coordinate(0, 0)
        val obstaclePositions = input.flatMapIndexed { y, line ->
            line.mapIndexedNotNull { x, char ->
                when (char) {
                    '^' -> {
                        guardPos = Coordinate(x, y)
                        null
                    }
                    '#' -> Coordinate(x, y)
                    else -> null
                }
            }
        }
        val mapBoundaries = Coordinate(0,0) to Coordinate(input.first().length - 1, input.size - 1)
        val map = LabMap(obstaclePositions.toSet(), mapBoundaries)
        val guard = Guard(guardPos, CardinalDirection.North)
        return map to guard
    }

    override fun part1(input: List<String>): String {
        val (map, guard) = parseInput(input)
        val result = guard.traverseMap(map)
        return result.visited.size.toString()
    }

    override fun part2(input: List<String>): String {
        val (map, guard) = parseInput(input)
        val initialResult = guard.traverseMap(map)
        val newMaps = initialResult.visited
            .filter { it != guard.startPos }
            .map { map.copy(obstacles = map.obstacles + it) }
        return newMaps.count {
            guard.traverseMap(it) is TraverseResult.WalkedInLoop
        }.toString()
    }

}

private data class LabMap(val obstacles: Set<Coordinate>, val mapBoundaries: Pair<Coordinate, Coordinate>) {
    fun isObstacle(coordinate: Coordinate): Boolean {
        return coordinate in obstacles
    }

    fun isWithinBounds(coordinate: Coordinate): Boolean {
        return coordinate.x in mapBoundaries.first.x..mapBoundaries.second.x
                && coordinate.y in mapBoundaries.first.y..mapBoundaries.second.y
    }
}

private data class Guard(
    val startPos: Coordinate,
    val startDirection: CardinalDirection
) {
    fun traverseMap(map: LabMap): TraverseResult {
        var curPosDir = startPos to startDirection
        val visitedWithDirection = mutableSetOf(curPosDir)

        while(true) {
            curPosDir = step(map, curPosDir.first, curPosDir.second)
            if(!map.isWithinBounds(curPosDir.first)) {
                return TraverseResult.WalkedOut(visitedWithDirection.map { it.first }.toSet())
            }
            if(!visitedWithDirection.add(curPosDir)) {
                return TraverseResult.WalkedInLoop(visitedWithDirection.map { it.first }.toSet())
            }
        }
    }

    private fun step(
        map: LabMap,
        curPos: Coordinate,
        curDirection: CardinalDirection
    ): Pair<Coordinate, CardinalDirection> {
        val nextPos = curPos + curDirection.toCoordinate()
        if (map.isObstacle(nextPos)) {
            return step(map, curPos, curDirection.turnRight())
        }
        return nextPos to curDirection
    }

}

private sealed class TraverseResult(val visited: Set<Coordinate>) {
    class WalkedOut(visited: Set<Coordinate>): TraverseResult(visited)
    class WalkedInLoop(visited: Set<Coordinate>): TraverseResult(visited)
}