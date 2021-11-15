class Day2 : Day {
    override fun part1(input: List<String>): String {
        val intcode = Intcode(input)
        intcode.write(1, 12, ParameterMode.IMMEDIATE)
        intcode.write(2, 2, ParameterMode.IMMEDIATE)
        return intcode.runProgram().toString()
    }

    override fun part2(input: List<String>): String {
        val possibleValues = (0L..99L).toList()
        val possibilities = possibleValues.cartesianProduct(possibleValues)
        val correctResult = possibilities.find {
            val intcode = Intcode(input)
            intcode.write(1, it.first, ParameterMode.IMMEDIATE)
            intcode.write(2, it.second, ParameterMode.IMMEDIATE)
            intcode.runProgram() == 19690720L
        } ?: return "No correct result found"

        return correctResult.first.toString() + correctResult.second.toString()
    }
}