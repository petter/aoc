class Day4 : Day {
    override fun part1(input: List<String>): String {
        val parsedInput = parseInput(input)
        val possiblePasswords = (parsedInput.first..parsedInput.second)
        return possiblePasswords.filter { validatePassword(it) }.size.toString()
    }

    override fun part2(input: List<String>): String {
        val parsedInput = parseInput(input)
        val possiblePasswords = (parsedInput.first..parsedInput.second)
        return possiblePasswords.filter { validatePassword2(it) }.size.toString()
    }

    private fun validatePassword(password: Int) : Boolean {
        val criteria = listOf(Day4::isCorrectLength, Day4::hasTwoIdenticalAdjacentDigits, Day4::numbersDontDecrease)
        return criteria.all { it(this, password) }
    }

    private fun validatePassword2(password: Int) : Boolean {
        val criteria = listOf(
            Day4::isCorrectLength,
            Day4::hasTwoIdenticalAdjacentDigits,
            Day4::numbersDontDecrease,
            Day4::hasEvenNumberOfIdenticalNumbers
        )
        return criteria.all { it(this, password) }
    }

    private fun isCorrectLength(input: Int): Boolean {
        return input.toString().length == 6
    }

    private fun hasTwoIdenticalAdjacentDigits(input: Int): Boolean {
        val inputString = input.toString()
        val identicalAdjacentDigits = inputString.zipWithNext().find { it.first == it.second }
        return identicalAdjacentDigits != null
    }

    private fun numbersDontDecrease(input: Int): Boolean {
        val inputString = input.toString()
        val numbers = inputString.map { it.toString().toInt() }
        return numbers.zipWithNext().all { it.first <= it.second }
    }

    private fun hasEvenNumberOfIdenticalNumbers(input: Int): Boolean {
        val inputString = input.toString()
        val numbers = inputString.map { it.toString().toInt() }
        val numberCounts = numbers.groupingBy { it }.eachCount()
        return numberCounts.any { it.value == 2 }
    }

    private fun parseInput(input: List<String>): Pair<Int, Int> {
        return input.first().split("-").map { it.toInt() }.let { Pair(it[0], it[1]) }
    }
}
