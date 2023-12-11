class Day11 : Day {

    private fun parseInput(input: List<String>): Universe {
        var galaxyId = 1
        val galaxyMap = input.flatMapIndexed { y, line ->
            line.mapIndexed { x, char ->
                when (char) {
                    '.' -> null
                    '#' -> Coordinate(x, y) to Galaxy(galaxyId++)
                    else -> throw IllegalArgumentException("Unknown character $char")
                }
            }.filterNotNull()
        }.toMap()
        val universeSize = Coordinate(input[0].length, input.size)

        return Universe(galaxyMap, universeSize)
    }

    override fun part1(input: List<String>): String {
        val universe = parseInput(input)
        universe.expand(1)
        return universe.distanceBetweenGalaxies().toString()
    }

    override fun part2(input: List<String>): String {
        val universe = parseInput(input)
        universe.expand(999999)
        return universe.distanceBetweenGalaxies().toString()
    }

}

private class Universe(var galaxies: Map<Coordinate, Galaxy>, val galaxySize: Coordinate) {

    fun expand(expandBy: Int) {
        val galaxyRows = galaxies.keys.map { it.y }.toSet()
        val emptyRows = (0..galaxySize.y).filter { it !in galaxyRows }
        val galaxyColumns = galaxies.keys.map { it.x }.toSet()
        val emptyColumns = (0..galaxySize.x).filter { it !in galaxyColumns }

        galaxies = galaxies.entries.associate { (coord, galaxy) ->
            val emptyRowsBeforeMe = emptyRows.filter { it < coord.y }.size
            val emptyColumnsBeforeMe = emptyColumns.filter { it < coord.x }.size
            coord + Coordinate(emptyColumnsBeforeMe, emptyRowsBeforeMe) * expandBy to galaxy
        }
    }

    fun distanceBetweenGalaxies(): Long {
        val distanceBetweenGalaxyPairs = galaxies.entries.flatMap { (coord1, galaxy1) ->
            galaxies.mapNotNull { (coord2, galaxy2) ->
                if (galaxy2.id > galaxy1.id) manhattanDistance(
                    coord1.y.toLong() to coord1.x.toLong(),
                    coord2.y.toLong() to coord2.x.toLong()
                ) else null
            }
        }

        return distanceBetweenGalaxyPairs.sum()
    }

}

private data class Galaxy(val id: Int)