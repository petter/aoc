class Day2 : Day {

    private fun parseInput(input: List<String>): List<Game> {
        return input.map { line ->
            val gameId = line.substringAfter("Game ").substringBefore(":").toInt()

            val cubeSets = line.substringAfter("Game $gameId: ").split("; ").map { cubeSet ->
                cubeSet.split(", ").map { cubeSetEntry ->
                    val color = when (cubeSetEntry.substringAfter(" ")) {
                        "red" -> CubeColor.RED
                        "green" -> CubeColor.GREEN
                        "blue" -> CubeColor.BLUE
                        else -> throw IllegalArgumentException("Unknown color ${cubeSetEntry.substringBefore(" ")}")
                    }
                    val count = cubeSetEntry.substringBefore(" ").toInt()
                    CubeSetEntry(color, count)
                }
            }

            Game(gameId, cubeSets)
        }
    }

    override fun part1(input: List<String>): String {
        val games = parseInput(input)

        return games
            .filter { isValidCubeCount(it) }
            .sumOf { (gameId) -> gameId }
            .toString()
    }

    private fun getCubeCount(input: Game): Map<CubeColor, Int> {
        val highestCubeCount = mutableMapOf(
            CubeColor.RED to 0,
            CubeColor.GREEN to 0,
            CubeColor.BLUE to 0
        )

        input.cubeSets.forEach { cubeSet ->
            cubeSet.forEach { cubeSetEntry ->
                if (cubeSetEntry.count > highestCubeCount[cubeSetEntry.color]!!) {
                    highestCubeCount[cubeSetEntry.color] = cubeSetEntry.count
                }
            }
        }

        return highestCubeCount
    }

    private fun isValidCubeCount(game: Game): Boolean {
        val cubeCount = getCubeCount(game)

        return cubeCount[CubeColor.RED]!! <= 12 &&
                cubeCount[CubeColor.GREEN]!! <= 13 &&
                cubeCount[CubeColor.BLUE]!! <= 14
    }


    override fun part2(input: List<String>): String {
        val games = parseInput(input)

        return games.sumOf { getGameCubePower(it) }.toString()
    }

    private fun getGameCubePower(game: Game): Int {
        val cubeCount = getCubeCount(game)

        return cubeCount[CubeColor.RED]!! *
                cubeCount[CubeColor.GREEN]!! *
                cubeCount[CubeColor.BLUE]!!
    }
}

private data class Game(val id: Int, val cubeSets: List<List<CubeSetEntry>>)

private data class CubeSetEntry(val color: CubeColor, val count: Int)

private enum class CubeColor {
    RED, GREEN, BLUE
}