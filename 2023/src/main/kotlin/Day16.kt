class Day16 : Day {
    private fun parseInput(input: List<String>): Map<Coordinate, Tile> {
        return input.flatMapIndexed { y, line ->
            line.mapIndexed { x, c ->
                val coordinate = Coordinate(x, y)
                coordinate to when(c) {
                    '.' -> EmptyTile(coordinate)
                    '/' -> MirrorTile('/', coordinate)
                    '\\' -> MirrorTile('\\', coordinate)
                    '-' -> SplitterTile(false, coordinate)
                    '|' -> SplitterTile(true, coordinate)
                    else -> throw IllegalArgumentException("Unknown tile type: $c")
                }
            }
        }.toMap()
    }

    private fun step(beams: List<Beam>, tileMap: Map<Coordinate, Tile>): List<Beam> {
        return beams.flatMap { beam ->
            val tile = tileMap[beam.coordinate]!!
            tile.energized = true
            when(tile) {
                is EmptyTile -> {
                    listOf(beam)
                }

                is MirrorTile -> {
                    beam.direction = tile.getNewDirection(beam.direction)
                    listOf(beam)
                }

                is SplitterTile -> {
                    tile.getNewBeams(beam)
                }
            }
        }.asSequence()
            .onEach { it.move() }
            .filter { it.coordinate in tileMap }
            .toSet()
            .toList()
    }

    private fun printMap(tileMap: Map<Coordinate, Tile>, mapSize: Coordinate, showEnergized: Boolean = false) {
        for(y in 0..mapSize.y) {
            for(x in 0..mapSize.x) {
                val tile = tileMap[Coordinate(x, y)]!!
                if(showEnergized) {
                    print(if(tile.energized) '#' else '.')
                    continue
                }

                print(when(tile) {
                    is EmptyTile -> '.'
                    is MirrorTile -> tile.mirrorChar
                    is SplitterTile -> if(tile.isVertical) '|' else '-'
                })
            }
            println()
        }
    }

    private fun simulate(initialBeams: List<Beam>, tileMap: Map<Coordinate, Tile>) {
        var beams = initialBeams
        var i = 0L
        var lastEnergizedCount = 0
        while(beams.isNotEmpty()) {
            beams = step(beams, tileMap)
            if(i % 100 == 0L) {
                val energizedCount = tileMap.values.count { it.energized }
                if(energizedCount == lastEnergizedCount) {
                    break
                }
                lastEnergizedCount = energizedCount
            }
            i++
        }

    }

    private fun resetEnergizedStatus(tileMap: Map<Coordinate, Tile>) {
        tileMap.values.forEach { it.energized = false }
    }

    override fun part1(input: List<String>): String {
        val tileMap = parseInput(input)
        simulate(listOf(Beam(CardinalDirection.East, Coordinate(0, 0))), tileMap)
        return tileMap.values.count { it.energized }.toString()
    }

    override fun part2(input: List<String>): String {
        val tileMap = parseInput(input)
        val maxX = tileMap.keys.maxOf { it.x }
        val maxY = tileMap.keys.maxOf { it.y }
        val startPositions = listOf(
            (0..maxX).map { Coordinate(it, 0) },
            (0..maxX).map { Coordinate(it, maxY) },
            (0..maxY).map { Coordinate(0, it) },
            (0..maxY).map { Coordinate(maxX, it) },
        ).flatten()

        val startDirections = listOf(
            CardinalDirection.East,
            CardinalDirection.South,
            CardinalDirection.West,
            CardinalDirection.North,
        )

        val startBeams = startPositions.flatMap { pos ->
            startDirections.map { dir ->
                Beam(dir, pos)
            }
        }

        return startBeams.maxOf { beam ->
            resetEnergizedStatus(tileMap)
            simulate(listOf(beam), tileMap)
            tileMap.values.count { it.energized }
        }.toString()
    }

}

private data class Beam(
    var direction: CardinalDirection,
    var coordinate: Coordinate
) {
    fun move() {
        val newCoordinate = coordinate + direction.toCoordinate()
        coordinate = newCoordinate
    }
}

private sealed class Tile(val coordinate: Coordinate, var energized: Boolean = false)

private class EmptyTile(coordinate: Coordinate): Tile(coordinate)

private class SplitterTile(val isVertical: Boolean, coordinate: Coordinate): Tile(coordinate) {
    fun getNewBeams(beam: Beam): List<Beam> {
        if (isVertical) {
            return when (beam.direction) {
                CardinalDirection.East, CardinalDirection.West -> {
                    listOf(
                        beam.copy(direction = CardinalDirection.North),
                        beam.copy(direction = CardinalDirection.South),
                    )
                }

                else -> listOf(beam)
            }
        } else {
            return when (beam.direction) {
                CardinalDirection.North, CardinalDirection.South -> {
                    listOf(
                        beam.copy(direction = CardinalDirection.West),
                        beam.copy(direction = CardinalDirection.East),
                    )
                }

                else -> listOf(beam)
            }

        }
    }
}

private class MirrorTile(val mirrorChar: Char, coordinate: Coordinate): Tile(coordinate) {
    fun getNewDirection(prevDirection: CardinalDirection): CardinalDirection {
        when(mirrorChar) {
            '/' -> {
                return when(prevDirection) {
                    CardinalDirection.North -> CardinalDirection.East
                    CardinalDirection.East -> CardinalDirection.North
                    CardinalDirection.South -> CardinalDirection.West
                    CardinalDirection.West -> CardinalDirection.South
                }
            }
            '\\' -> {
                return when(prevDirection) {
                    CardinalDirection.North -> CardinalDirection.West
                    CardinalDirection.East -> CardinalDirection.South
                    CardinalDirection.South -> CardinalDirection.East
                    CardinalDirection.West -> CardinalDirection.North
                }
            }

            else -> throw IllegalArgumentException("Mirror char must be either '/' or '\\', was $mirrorChar")
        }
    }
}
