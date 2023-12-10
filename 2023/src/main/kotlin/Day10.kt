class Day10 : Day {

    private fun parseInput(input: List<String>): Map<Coordinate, PipePart> {
        return input.mapIndexed { y, line ->
            line.mapIndexed { x, symbol ->
                if (symbol == '.') {
                    null
                } else {
                    val coordinate = Coordinate(x, y)
                    val pipePartType = PipePartType.symbolToPipePart(symbol)
                    coordinate to PipePart(pipePartType, coordinate)
                }
            }
        }.flatten().filterNotNull().toMap()
    }

    private fun findLongestDistanceFrom(pipeMap: Map<Coordinate, PipePart>): Int {
        walkPipe(pipeMap) { pipe ->
            if(pipe.type == PipePartType.StartingPosition) {
                pipe.distance = 0
            } else {
                val prevPipe = pipe.nextPipe().firstOrNull { pipeMap[it]?.distance != null}.let { pipeMap[it] }
                if (prevPipe != null) {
                    pipe.distance = prevPipe.distance!! + 1
                }
            }
        }

        return pipeMap.values.map { it.distance }.maxBy { it ?: -1 } ?: throw IllegalArgumentException("No pipes found")
    }

    private fun walkPipe(pipeMap: Map<Coordinate, PipePart>, action: (PipePart) -> Unit) {
        val startPipe = pipeMap.values.first { it.type == PipePartType.StartingPosition }
        action(startPipe)
        val startCoordinate = startPipe.coordinate
        val neighbourCoordinates = startCoordinate.neighbours()
        val neighbourPipes = neighbourCoordinates.mapNotNull { pipeMap[it] }
        val visitedPipes = mutableMapOf(startCoordinate to true)
        var pipeQueue = neighbourPipes
            .filter { it.nextPipe().contains(startCoordinate) }
            .map { it.coordinate }

        while (pipeQueue.isNotEmpty()) {
            pipeQueue = pipeQueue.mapNotNull { coord ->
                val curPipe = pipeMap[coord] ?: throw IllegalArgumentException("No pipe at $coord")
                action(curPipe)
                visitedPipes[coord] = true
                val next = curPipe.nextPipe().firstOrNull { !visitedPipes.getOrDefault(it, false) }
                next
            }
        }
    }

    private fun getStartPositionType(pipeMap: Map<Coordinate, PipePart>) : Pair<Coordinate, PipePart> {
        val startPipe = pipeMap.values.first { it.type == PipePartType.StartingPosition }
        val startCoordinate = startPipe.coordinate
        val neighbourCoordinates = startCoordinate.neighbours()
        val neighbourPipes = neighbourCoordinates.mapNotNull { pipeMap[it] }
        var pipesPointingAtStart = neighbourPipes
            .filter { it.nextPipe().contains(startCoordinate) }
            .map { it.coordinate - startCoordinate }

        val type = when {
            pipesPointingAtStart.contains(Coordinate.UP) && pipesPointingAtStart.contains(Coordinate.DOWN) -> PipePartType.NorthSouth
            pipesPointingAtStart.contains(Coordinate.DOWN) && pipesPointingAtStart.contains(Coordinate.RIGHT) -> PipePartType.NorthEast
            pipesPointingAtStart.contains(Coordinate.UP) && pipesPointingAtStart.contains(Coordinate.RIGHT) -> PipePartType.SouthEast
            pipesPointingAtStart.contains(Coordinate.DOWN) && pipesPointingAtStart.contains(Coordinate.LEFT) -> PipePartType.NorthWest
            pipesPointingAtStart.contains(Coordinate.UP) && pipesPointingAtStart.contains(Coordinate.LEFT) -> PipePartType.SouthWest
            pipesPointingAtStart.contains(Coordinate.RIGHT) && pipesPointingAtStart.contains(Coordinate.LEFT) -> PipePartType.EastWest
            else -> throw Error("Could not determine start position type. Pipes pointing at start: $pipesPointingAtStart")
        }

        return startCoordinate to PipePart(type, startCoordinate)
    }

    override fun part1(input: List<String>): String {
        val pipeMap = parseInput(input)
        return findLongestDistanceFrom(pipeMap).toString()
    }

    override fun part2(input: List<String>): String {
        val pipeMap = parseInput(input).toMutableMap()
        walkPipe(pipeMap) { it.isMainLoop = true }
        val (startCoordinate, startPart) = getStartPositionType(pipeMap)
        pipeMap[startCoordinate] = startPart
        startPart.isMainLoop = true

        val yMax = pipeMap.keys.maxBy { it.y }.y
        val xMax = pipeMap.keys.maxBy { it.x }.x
        var y = 0
        var pointsInside = 0
        while(y <= yMax) {
            var x = -1
            var edgeHits = 0
            while(x <= xMax) {
                val pipe = pipeMap[Coordinate(++x, y)]
                if(pipe == null || !pipe.isMainLoop) {
                    if(edgeHits % 2 == 1) {
                        pointsInside++
                    }
                    continue
                }

                if(pipe != null && pipe.isMainLoop) {
                    if (pipe.type == PipePartType.NorthEast) {
                        while (pipeMap[Coordinate(++x, y)]?.type == PipePartType.EastWest) {
                        }
                        val edgeEndPipe = pipeMap[Coordinate(x, y)]!!
                        if (edgeEndPipe.type == PipePartType.NorthWest) {
                            continue
                        }
                    }

                    if (pipe.type == PipePartType.SouthEast) {
                        while (pipeMap[Coordinate(++x, y)]?.type == PipePartType.EastWest) {
                        }
                        val edgeEndPipe = pipeMap[Coordinate(x, y)]!!
                        if (edgeEndPipe.type == PipePartType.SouthWest) {
                            continue
                        }
                    }


                    edgeHits++

                }
            }
            y++
        }
        return pointsInside.toString()
    }

    private fun PipePart.canBeStartStopPipe(): Boolean {
        return when(type) {
            PipePartType.NorthSouth -> true
            PipePartType.NorthWest -> true
            PipePartType.NorthEast -> true
            PipePartType.SouthWest -> true
            PipePartType.SouthEast -> true
            else -> false
        }
    }

}

private class PipePart(val type: PipePartType, val coordinate: Coordinate) {
    var distance : Int? = null
    var isMainLoop = false
    fun nextPipe(): List<Coordinate> {
        return when (type) {
            PipePartType.NorthSouth -> listOf(coordinate + Coordinate.UP, coordinate + Coordinate.DOWN)
            PipePartType.EastWest -> listOf(coordinate + Coordinate.LEFT, coordinate + Coordinate.RIGHT)
            PipePartType.NorthEast -> listOf(coordinate + Coordinate.DOWN, coordinate + Coordinate.RIGHT)
            PipePartType.NorthWest -> listOf(coordinate + Coordinate.DOWN, coordinate + Coordinate.LEFT)
            PipePartType.SouthWest -> listOf(coordinate + Coordinate.UP, coordinate + Coordinate.LEFT)
            PipePartType.SouthEast -> listOf(coordinate + Coordinate.UP, coordinate + Coordinate.RIGHT)
            PipePartType.StartingPosition -> throw IllegalArgumentException("Cannot get next pipe from starting position")
        }
    }
}
private enum class PipePartType(val symbol: Char) {
    NorthSouth('|'),
    EastWest('-'),
    NorthEast('L'),
    NorthWest('J'),
    SouthWest('7'),
    SouthEast('F'),
    StartingPosition('S');

    companion object {
        fun symbolToPipePart(symbol: Char): PipePartType {
            PipePartType.entries.find { it.symbol == symbol }?.let { return it }
            throw IllegalArgumentException("Unknown symbol $symbol")
        }
    }
}