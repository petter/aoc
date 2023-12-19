class Day18 : Day {

    private fun parseInput(input: List<String>): List<Instruction> {
        return input.map { line ->
            val (directionString, lengthString, hexString) = line.split(" ")
            val direction = when(directionString) {
                "U" -> CardinalDirection.North
                "D" -> CardinalDirection.South
                "L" -> CardinalDirection.West
                "R" -> CardinalDirection.East
                else -> throw Error("Unknown direction $directionString")
            }
            val length = lengthString.toInt()
            val hex = hexString.substringAfter("(#").substringBefore(")")
            Instruction(direction, length, hex)
        }
    }

    private fun getShapePoints(instructions: List<Instruction>): List<Coordinate> {
        var pos = Coordinate(0, 0)
        val points = mutableListOf(pos)
        for(instruction in instructions) {
            val directionCoord = instruction.direction.toCoordinate()
            pos += directionCoord * instruction.length
            points.add(pos)
        }

        return points
    }

    override fun part1(input: List<String>): String {
        val instructions = parseInput(input)
        val shape = getShapePoints(instructions)
        val interiorArea = pickTheoremForInteriorPoints(shoelaceArea(shape).toLong(), circumference(shape))
        return (interiorArea + circumference(shape)).toString()
    }

    private fun transformInstructionForPart2(instruction: Instruction): Instruction {
        val length = instruction.color.substring(0, 5).toInt(16)
        val direction = when(instruction.color.substring(5)) {
            "0" -> CardinalDirection.East
            "1" -> CardinalDirection.South
            "2" -> CardinalDirection.West
            "3" -> CardinalDirection.North
            else -> throw Error("Wrong direction")
        }

        return Instruction(direction, length, "")
    }

    override fun part2(input: List<String>): String {
        val instructions = parseInput(input)
        val newInstructions = instructions.map(::transformInstructionForPart2)
        val shape = getShapePoints(newInstructions)
        val interiorArea = pickTheoremForInteriorPoints(shoelaceArea(shape).toLong(), circumference(shape))
        return (interiorArea + circumference(shape)).toString()
    }

}

private data class Instruction(val direction: CardinalDirection, val length: Int, val color: String)
