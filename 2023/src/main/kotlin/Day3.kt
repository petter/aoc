class Day3 : Day {

    override fun part1(input: List<String>): String {
        val engineSchematic = EngineSchematic(input)
        val symbols = engineSchematic.getAllSymbols()
        return symbols.sumOf {
            it.getRelatedNumbers(engineSchematic).sumOf { num -> num.value }
        }.toString()
    }

    override fun part2(input: List<String>): String {
        val engineSchematic = EngineSchematic(input)
        val symbols = engineSchematic.getAllSymbols()
        val gears = symbols.filter { it.value == '*' && it.getRelatedNumbers(engineSchematic).size == 2 }
        return gears.sumOf { gear ->
            gear.getRelatedNumbers(engineSchematic).map { num -> num.value }.reduce(Int::times)
        }.toString()
    }

}

private class EngineSchematic(input: List<String>) {
    private val engineMap: Map<Coordinate, EnginePart>
    init {
        val mutableEngineMap = mutableMapOf<Coordinate, EnginePart>()
        for ((y, line) in input.withIndex()) {
            var x = 0
            while(x < line.length) {
                val char = line[x]

                if(char == '.') {
                    x++
                    continue
                }

                if(char.isDigit()) {
                    val numberStart = x
                    var numberEnd = x++
                    while(x < line.length && line[x].isDigit()) {
                        numberEnd = x
                        x++
                    }

                    val numberValue = line.substring(numberStart, numberEnd + 1).toInt()
                    val xPosRange = numberStart.toLong()..numberEnd.toLong()
                    val number = Number(numberValue, y.toLong(), xPosRange)

                    for(numberX in xPosRange) {
                        mutableEngineMap[Coordinate(numberX, y.toLong())] = number
                    }

                    continue
                }

                mutableEngineMap[Coordinate(x, y)] = Symbol(char, y.toLong(), x.toLong())
                x++
            }
        }

        engineMap = mutableEngineMap
    }

    fun getPartNeighbours(part: EnginePart): List<EnginePart> {
        fun getNeighbourCoords(x: Long, y: Long): List<Coordinate> {
            return listOf(
                Coordinate(x, y - 1),
                Coordinate(x - 1, y - 1),
                Coordinate(x + 1, y - 1),
                Coordinate(x - 1, y),
                Coordinate(x + 1, y),
                Coordinate(x, y + 1),
                Coordinate(x - 1, y + 1),
                Coordinate(x + 1, y + 1),
            )
        }

        val neighbourCoords = when (part) {
            is Symbol -> getNeighbourCoords(part.x, part.y)
            is Number -> {
                part.x.flatMap { x -> getNeighbourCoords(x, part.y) }
                    .filter { !(it.y == part.y && it.x in part.x) } // Remove coordinates that are part of the number
            }
        }

        return neighbourCoords.mapNotNull { engineMap[it] }.toSet().toList() // Remove duplicates
    }

    fun getAllSymbols() : List<Symbol> {
        return engineMap.values.filterIsInstance<Symbol>()
    }
}

private sealed class EnginePart

private data class Symbol(val value: Char, val y: Long, val x: Long) : EnginePart() {
    fun getRelatedNumbers(engineSchematic: EngineSchematic): List<Number> {
        return engineSchematic.getPartNeighbours(this).filterIsInstance<Number>()
    }
}

private data class Number(val value: Int, val y: Long, val x: LongRange) : EnginePart()
