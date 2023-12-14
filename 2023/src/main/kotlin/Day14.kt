

class Day14 : Day {

    private fun parseInput(input: List<String>): Pair<Map<Coordinate, Rock>, Coordinate>  {
        return input.flatMapIndexed { y, line ->
            line.mapIndexedNotNull { x, c ->
                when (c) {
                    'O' -> Coordinate(x, y) to Rock.Round
                    '#' -> Coordinate(x, y) to Rock.Square
                    '.' -> null
                    else -> throw IllegalArgumentException("Unknown space type: $c")
                }
            }
        }.toMap() to Coordinate(input[0].length - 1, input.size - 1)
    }

    private fun tilt(
        direction: CardinalDirection,
        map: Map<Coordinate, Rock>,
        mapSize: Coordinate
    ): Map<Coordinate, Rock> {
        val fallRow = when (direction) {
            CardinalDirection.North, CardinalDirection.South -> 0..mapSize.x
            CardinalDirection.West, CardinalDirection.East -> 0..mapSize.y
        }

        val fallColumn = when (direction) {
            CardinalDirection.North -> 0..mapSize.y
            CardinalDirection.South -> mapSize.y downTo 0
            CardinalDirection.East -> mapSize.x downTo  0
            CardinalDirection.West -> 0..mapSize.x
        }

        val newMap = mutableMapOf<Coordinate, Rock>()
        for (rowPos in fallRow) {
            var nextFree = fallColumn.first
            for (colPos in fallColumn) {
                val coordinate = when (direction) {
                    CardinalDirection.North, CardinalDirection.South -> Coordinate(rowPos, colPos)
                    CardinalDirection.East, CardinalDirection.West -> Coordinate(colPos, rowPos)
                }

                val rock = map[coordinate]
                if (rock == Rock.Round) {
                    val newPos = when(direction) {
                        CardinalDirection.North -> Coordinate(rowPos, nextFree++)
                        CardinalDirection.South -> Coordinate(rowPos, nextFree--)
                        CardinalDirection.East -> Coordinate(nextFree--, rowPos)
                        CardinalDirection.West -> Coordinate(nextFree++, rowPos)
                    }
                    newMap[newPos] = rock
                } else if(rock == Rock.Square) {
                    newMap[coordinate] = rock
                    nextFree = when(direction) {
                        CardinalDirection.North, CardinalDirection.West -> colPos + 1
                        CardinalDirection.South, CardinalDirection.East -> colPos - 1
                    }
                }
            }
        }

        return newMap
    }

    private fun printMap(map: Map<Coordinate, Rock>, mapSize: Coordinate) {
        for (y in 0..mapSize.y) {
            for (x in 0..mapSize.x) {
                val coordinate = Coordinate(x, y)
                val rock = map[coordinate]
                when(rock) {
                    Rock.Round -> print('O')
                    Rock.Square -> print('#')
                    else -> print('.')
                }
            }
            println()
        }
    }

    private fun getNorthTotalLoad(map: Map<Coordinate, Rock>, mapSize: Coordinate): Long {
        var totalLoad = 0L
        for (y in 0..mapSize.y) {
            for (x in 0..mapSize.x) {
                val coordinate = Coordinate(x, y)
                val rock = map[coordinate]
                if (rock == Rock.Round) {
                    totalLoad += (mapSize.y + 1) - y
                }
            }
        }

        return totalLoad
    }

    private fun cycle(map: Map<Coordinate, Rock>, mapSize: Coordinate) : Map<Coordinate, Rock> {
        return listOf(
            CardinalDirection.North,
            CardinalDirection.West,
            CardinalDirection.South,
            CardinalDirection.East,
        ).fold(map) { acc, direction ->
            tilt(direction, acc, mapSize)
        }
    }

    override fun part1(input: List<String>): String {
        val (map, mapSize) = parseInput(input)
        val northTiltedMap = tilt(CardinalDirection.North, map, mapSize)

        return getNorthTotalLoad(northTiltedMap, mapSize).toString()
    }

    override fun part2(input: List<String>): String {
        val (map, mapSize) = parseInput(input)
        val cycleGoal = 1000000000L

        val cycleMap = mutableMapOf<Map<Coordinate, Rock>, Long>()
        var lastMap = map
        var cycleIndex: Long? = null
        var cycleLength: Long? = null
        var i = 0L
        while (i < cycleGoal) {
            lastMap = cycle(lastMap, mapSize)

            if (lastMap in cycleMap) {
                val lastMapIndex = cycleMap[lastMap]!!
                cycleIndex = i
                cycleLength = i - lastMapIndex
                break
            }

            cycleMap[lastMap] = i
            i++
        }

        if(cycleIndex != null && cycleLength != null) {

            val cycleRepeatTimes = (cycleGoal - cycleIndex) / cycleLength
            val curIndex = cycleRepeatTimes * cycleLength + cycleIndex + 1
            val cycledMap = (curIndex..<cycleGoal).fold(lastMap) { acc, _ ->
                cycle(acc, mapSize)
            }
            return getNorthTotalLoad(cycledMap, mapSize).toString()
        }

        return getNorthTotalLoad(lastMap, mapSize).toString()
    }

}

enum class Rock {
    Round, Square
}