class Day04 : Day {
    private fun parseInput(input: List<String>): Map<Coordinate, Char> {
        return input.mapIndexed { y, row ->
            row.mapIndexed { x, c ->
                Coordinate(x, y) to c
            }
        }.flatten().toMap()
    }

    override fun part1(input: List<String>): String {
        val map = parseInput(input)
        val searchWord = "XMAS"
        val startPositions = map.filter { it.value == searchWord.first() }.keys
        val wordSearchDirections = listOf(
            Coordinate.UP,
            Coordinate.DOWN,
            Coordinate.LEFT,
            Coordinate.RIGHT,
            Coordinate.UP + Coordinate.LEFT,
            Coordinate.UP + Coordinate.RIGHT,
            Coordinate.DOWN + Coordinate.LEFT,
            Coordinate.DOWN + Coordinate.RIGHT,
        )

        var foundWords = 0
        for (start in startPositions) {
            for (direction in wordSearchDirections) {
                var currentPos = start
                var currentWord = ""
                while (currentWord.length < searchWord.length) {
                    currentWord += map[currentPos] ?: break
                    if (currentWord == searchWord) {
                        foundWords++
                        break
                    }
                    currentPos += direction
                }
            }
        }

        return foundWords.toString()
    }

    override fun part2(input: List<String>): String {
        val map = parseInput(input)
        val searchWord = "MAS"
        val startPositions = map.filter { it.value == searchWord[1] }.keys
        val wordSearchPatterns = listOf(
            listOf(Coordinate.DOWN + Coordinate.LEFT, Coordinate(0, 0), Coordinate.UP + Coordinate.RIGHT),
            listOf(Coordinate.DOWN + Coordinate.RIGHT, Coordinate(0, 0), Coordinate.UP + Coordinate.LEFT),
        )

        val foundWords = startPositions.count { start ->
            wordSearchPatterns.map { wordSearch ->
                wordSearch.map { map[it + start] }.joinToString("")
            }.all { it == searchWord || it == searchWord.reversed() }
        }

        return foundWords.toString()
    }

}