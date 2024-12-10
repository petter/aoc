class Day07 : Day {
    private fun parseInput(input: List<String>): List<Pair<Long, List<Long>>> {
        return input.map { line ->
            val (total, nums) = line.split(": ")
            total.toLong() to nums.split(" ").map { it.toLong() }
        }
    }

    private fun isTrue(total: Long, nums: List<Long>, operators: List<Operator>): Boolean {
        if(nums.size == 1) return nums.first() == total
        if(nums.first() > total || nums.isEmpty()) return false

        val nextNums = nums.take(2)
        val rest = nums.drop(2)
        return operators.any { op ->
            isTrue(
                total,
                listOf(op.apply(nextNums[0], nextNums[1])) + rest,
                operators
            )
        }
    }

    override fun part1(input: List<String>): String {
        val equations = parseInput(input)
        val operators = listOf(Operator.Add, Operator.Mul)
        val trueEquations = equations.filter { (total, nums) -> isTrue(total, nums, operators) }
        return trueEquations.sumOf { it.first }.toString()
    }


    override fun part2(input: List<String>): String {
        val equations = parseInput(input)
        val operators = listOf(Operator.Add, Operator.Mul, Operator.Mul, Operator.Concat)
        val trueEquations = equations.filter { (total, nums) -> isTrue(total, nums, operators) }
        return trueEquations.sumOf { it.first }.toString()
    }
}

private sealed class Operator {
    abstract fun apply(a: Long, b: Long): Long

    data object Add : Operator() {
        override fun apply(a: Long, b: Long): Long {
            return a + b
        }
    }
    data object Mul : Operator() {
        override fun apply(a: Long, b: Long): Long {
            return a * b
        }
    }
    data object Concat : Operator() {
        override fun apply(a: Long, b: Long): Long {
            return "$a$b".toLong()
        }
    }
}