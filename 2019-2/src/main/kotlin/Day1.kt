class Day1 : Day {
    override fun part1(input: List<String>): String {
        val inputParsed = parseToInt(input)
        val fuelRequired = inputParsed.sumOf { fuelRequiredForModule(it) }
        return fuelRequired.toString()
    }

    override fun part2(input: List<String>): String {
        val inputParsed = parseToInt(input)
        val fuelRequiredForModule = inputParsed.map { fuelRequiredForModule(it) }
        return fuelRequiredForModule.sumOf { it + fuelRequiredForAllFuel(it) }.toString()
    }
}

fun fuelRequiredForModule(mass: Int): Int = Math.floorDiv(mass, 3) - 2

fun fuelRequiredForAllFuel(mass: Int): Int {
    val fuelRequired = fuelRequiredForModule(mass)

    return if (fuelRequired > 0) {
        fuelRequired + fuelRequiredForAllFuel(fuelRequired)
    } else {
        0
    }
}
