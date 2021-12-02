class Day2 : Day {
    override fun part1(input: List<String>): String {
        val submarine = Submarine()
        submarine.doMoves(input)
        return submarine.getAnswer()
    }

    override fun part2(input: List<String>): String {
        val submarine = Submarine()
        submarine.doMoves2(input)
        return submarine.getAnswer()
    }
}

class Submarine {
    var depth = 0
    var aim = 0
    var horizontal = 0

    fun doMoves(moves: List<String>) {
        moves.forEach {
            val (direction, amountString) = it.split(' ')
            val amount = amountString.toInt()
            when (direction) {
                "up" -> depth-=amount
                "down" -> depth+=amount
                "forward" -> horizontal+=amount
                else -> throw Error("Unknown direction $direction")
            }
        }
    }

    fun doMoves2(moves: List<String>) {
        moves.forEach {
            val (direction, amountString) = it.split(' ')
            val amount = amountString.toInt()
            when (direction) {
                "up" -> aim-=amount
                "down" -> aim+=amount
                "forward" -> {
                    horizontal+=amount
                    depth += aim*amount
                }
                else -> throw Error("Unknown direction $direction")
            }
        }
    }

    fun getAnswer () = (horizontal * depth).toString()

}