class Day9 : Day {
    override fun part1(input: List<String>): String {
        val heightMap = input.map { it.toCharArray().map { num -> num.toString().toInt() } }
        val lowPoints = lowPoints(heightMap).map { (x, y) -> heightMap[y][x] }
        return lowPoints.sumOf { it + 1 }.toString()
    }

    private fun lowPoints(heightMap: List<List<Int>>): List<Pair<Int, Int>> {
        val lowPoints = mutableListOf<Pair<Int, Int>>()
        for (y in heightMap.indices) {
            for (x in 0 until heightMap[y].size) {
                val num = heightMap[y][x]
                if(x != 0 && num >= heightMap[y][x - 1]) {
                    continue
                }
                if(x < heightMap[y].size - 1 && num >= heightMap[y][x + 1]) {
                    continue
                }
                if(y != 0 && num >= heightMap[y - 1][x]) {
                    continue
                }
                if(y < heightMap.size - 1 && num >= heightMap[y + 1][x]) {
                    continue
                }
                lowPoints.add(Pair(x, y))
            }
        }
        return lowPoints.toList()
    }

    // 176526168 - too high
    override fun part2(input: List<String>): String {
        val heightMap = input.map { it.toCharArray().map { num -> num.toString().toInt() } }
        val lowPoints = lowPoints(heightMap)
        val basins = lowPoints.map { getBasin(heightMap, it) }
        return basins
            .sortedBy { -it.size }
            .take(3)
            .fold(1) { acc, basin -> acc * basin.size }
            .toString()
    }

    private fun getBasin(heightMap: List<List<Int>>, lowPoint: Pair<Int, Int>): List<Int> {
        val basin = mutableListOf<Int>()
        val visitedPoints = mutableListOf<Pair<Int, Int>>()

        fun visit(pos: Pair<Int, Int>) {
            if(!heightMap.isValidPoint(pos)) {
                return
            }

            val num = heightMap[pos.second][pos.first]
            if(num == 9) {
                return
            }

            if(visitedPoints.contains(pos)) {
                return
            }

            basin.add(num)
            visitedPoints.add(pos)
            visit(Pair(pos.first - 1, pos.second))
            visit(Pair(pos.first + 1, pos.second))
            visit(Pair(pos.first, pos.second - 1))
            visit(Pair(pos.first, pos.second + 1))
        }

        visit(lowPoint)

        return basin.toList()
    }
}