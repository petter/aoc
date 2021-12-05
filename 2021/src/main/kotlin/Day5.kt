class Day5 : Day {
    private fun inputParser(input: List<String>): List<Pair<Pair<Int, Int>, Pair<Int, Int>>> {
        return input.map { line ->
            line.split(" -> ")
                .map { pair ->
                    pair.split(",")
                        .map { it.toInt() }
                        .toPair()
                }.toPair()
        }
    }

    // 959018 too high
    override fun part1(input: List<String>): String {
        val coords = inputParser(input)
        val points = coords.fold(mutableMapOf<Pair<Int, Int>, Int>()) { map, (from, to) ->
            if(from.first == to.first || from.second == to.second) {
                generatePointsBetween(from, to).forEach { point ->
                    val cur = map.getOrDefault(point, 0)
                    map[point] = cur + 1
                }
            }
            map
        }
        return points.filter { it.value > 1 }.size.toString()
    }

    override fun part2(input: List<String>): String {
        val coords = inputParser(input)
        val points = coords.fold(mutableMapOf<Pair<Int, Int>, Int>()) { map, (from, to) ->
            generatePointsBetween(from, to).forEach { point ->
                val cur = map.getOrDefault(point, 0)
                map[point] = cur + 1
            }
            map
        }
        return points.filter { it.value > 1 }.size.toString()
    }
}