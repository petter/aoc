fun parseIntcodeProgram(input: List<String>) : MutableList<Long> = input.first().split(",").map { it.toLong() }.toMutableList()

fun parseToInt(input: List<String>) : List<Int> = input.map { it.toInt() }
