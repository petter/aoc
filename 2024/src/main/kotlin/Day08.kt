class Day08 : Day {
    private fun parseInput(input: List<String>): AntennaMap {
        val antennas = input.mapIndexed { y, line ->
            line.mapIndexedNotNull { x, char ->
                if(char == '.') null else Coordinate(x, y) to char
            }
        }.flatten().toMap()
        val mapBoundaries = Coordinate(0, 0) to Coordinate(input.first().length - 1, input.size - 1)
        return AntennaMap(antennas, mapBoundaries)
    }

    private fun calcAntiNodes(antennaMap: AntennaMap, withResonantHarmonics: Boolean = false): Set<Coordinate> {
        return antennaMap.antennas.entries
            .groupBy { it.value }
            .flatMap { (_, coords) ->
                coords.asSequence()
                    .flatMap { (coordA) -> coords.map { (coordB) -> coordA to coordB } }
                    .filter { (coordA, coordB) -> coordA != coordB }
                    .flatMap { antiNodesFor(it, antennaMap, withResonantHarmonics).toList() }
            }.toSet()
    }

    private fun antiNodesFor(
        coords: Pair<Coordinate, Coordinate>,
        antennaMap: AntennaMap,
        withResonantHarmonics: Boolean = false
    ): Set<Coordinate> {
        val (a, b) = coords
        val ab = b - a
        val antiNodes = mutableSetOf<Coordinate>()
        if(withResonantHarmonics) {
            antiNodes.add(a)
            antiNodes.add(b)
        }

        var nextCoord = b + ab
        while(!antennaMap.isOutsideBounds(nextCoord)) {
            antiNodes.add(nextCoord)
            nextCoord += ab

            if(!withResonantHarmonics) {
                break
            }
        }
        return antiNodes
    }

    override fun part1(input: List<String>): String {
        val antennaMap = parseInput(input)
        val antiNodes = calcAntiNodes(antennaMap)
        return antiNodes.size.toString()
    }

    override fun part2(input: List<String>): String {
        val antennaMap = parseInput(input)
        val antiNodes = calcAntiNodes(antennaMap, true)
        return antiNodes.size.toString()
    }

}

private data class AntennaMap(
    val antennas: Map<Coordinate, Char>,
    val mapBoundaries: Pair<Coordinate, Coordinate>
) {
    fun isOutsideBounds(coordinate: Coordinate): Boolean {
        return coordinate.x !in mapBoundaries.first.x..mapBoundaries.second.x
                || coordinate.y !in mapBoundaries.first.y..mapBoundaries.second.y
    }
}
