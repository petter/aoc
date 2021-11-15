class Day5 : Day {
    override fun part1(input: List<String>): String {
        val intcode = Intcode(input, mutableListOf("1"))
        intcode.runProgram()
        return intcode.outputBuffer.last().toString()
    }

    override fun part2(input: List<String>): String {
        val intcode = Intcode(input, mutableListOf())
        return "Not yet implemented"
    }
}