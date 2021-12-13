enum class FoldDirection {
    X,
    Y;
    companion object {
        fun fromString(s: String): FoldDirection {
            return when (s) {
                "x" -> X
                "y" -> Y
                else -> throw IllegalArgumentException("Unknown direction: $s")
            }
        }
    }
}

data class Fold(val direction: FoldDirection, val pos: Int)

class Day13 : Day {
    companion object {
        private fun parseInput(input: List<String>) : Pair<Set<Pair<Int, Int>>, List<Fold>> {
            val (pointInput, foldInstructions) = listSplit(input, "").toPair()

            val points = pointInput.map { it.split(",").map { num -> num.toInt() }.toPair() }.toSet()

            val folds = foldInstructions.map { instr ->
                val (dir, pos) = instr.split(" ").last().split("=").toPair()
                Fold(FoldDirection.fromString(dir), pos.toInt())
            }

            return points to folds
        }

        private fun foldPoints(points: Set<Pair<Int, Int>>, fold: Fold) : Set<Pair<Int, Int>> {
            return if(fold.direction == FoldDirection.Y) {
                foldUp(points, fold.pos)
            } else {
                foldLeft(points, fold.pos)
            }
        }

        private fun foldUp(points: Set<Pair<Int, Int>>, amount: Int) : Set<Pair<Int, Int>> {
            val (aboveFoldKeys, belowFoldKeys) = points.partition { it.second < amount }
            val mirroredKeys = belowFoldKeys.map { (x, y) ->
                val lengthFromFold = y - amount
                x to (amount - lengthFromFold)
            }
            return (aboveFoldKeys + mirroredKeys).toSet()
        }

        private fun foldLeft(points: Set<Pair<Int, Int>>, amount: Int) : Set<Pair<Int, Int>> {
            val (leftOfFoldKeys, rightOfFoldKeys) = points.partition { it.first < amount }
            val mirroredKeys = rightOfFoldKeys.map { (x, y) ->
                val lengthFromFold = x - amount
                (amount - lengthFromFold) to y
            }
            return (leftOfFoldKeys + mirroredKeys).toSet()

        }
    }

    override fun part1(input: List<String>): String {
        val (points, folds) = parseInput(input)
        val res = foldPoints(points, folds.first())
        return res.size.toString()
    }

    override fun part2(input: List<String>): String {
        val (points, folds) = parseInput(input)
        val res = folds.fold(points) { acc, fold -> foldPoints(acc, fold) }
        val maxX = res.maxOf { it.first }
        val maxY = res.maxOf { it.second }
        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (res.contains(x to y)) {
                    print("⬜")
                } else {
                    print("⬛")
                }
            }
            println()
        }
        return "0"
    }
}