import java.util.*

class Day17 : Day {
    private fun parseInput(input: List<String>): Map<Coordinate, Block> {
        val blocks = input.flatMapIndexed { y, line ->
            line.mapIndexed { x, c ->
                val coordinate = Coordinate(x, y)
                coordinate to Block(c.digitToInt(), coordinate)
            }
        }.toMap()

        blocks.values.forEach { block ->
            block.neighbours.addAll(block.coordinate.neighbours().mapNotNull { blocks[it] })
        }

        return blocks
    }

    private fun djikstra(start: Block, end: Block, minStepsStraight: Int, maxStepsStraight: Int): List<Block> {
        val queue = PriorityQueue<List<Block>>(10000, compareBy { it.totalHeatLoss() })
        val seen = mutableSetOf<Triple<Block, CardinalDirection, Int>>()
        start.neighbours.forEach {
            queue.add(listOf(start, it))
        }

        while (queue.isNotEmpty()) {
            val curPath = queue.poll()
            val current = curPath.last()

            val travelDirections = curPath
                .map { it.coordinate }
                .zipWithNext()
                .map { (it.second - it.first).toCardinalDirection() }

            val lastDirection = travelDirections.last()
            val stepsTakenStraight = travelDirections.takeLastWhile { it == lastDirection }.size

            if (current == end) {
                if(stepsTakenStraight < minStepsStraight) continue
                return curPath
            }

            val thisStep = Triple(current, lastDirection, stepsTakenStraight)
            if (thisStep in seen) {
                continue
            }
            seen.add(thisStep)

            val neighboursNotPartOfPath = current.neighbours.filter { it !in curPath }
            val nextBlocks = when {
                stepsTakenStraight < minStepsStraight -> neighboursNotPartOfPath.filter { it.coordinate == current.coordinate + lastDirection.toCoordinate() }
                stepsTakenStraight >= maxStepsStraight -> neighboursNotPartOfPath.filter { it.coordinate != current.coordinate + lastDirection.toCoordinate() }
                else -> neighboursNotPartOfPath
            }

            nextBlocks.forEach { next ->
                queue.add(curPath + next)
            }
        }

        throw Error("Could not find path")
    }

    override fun part1(input: List<String>): String {
        val blocks = parseInput(input)
        val start = blocks[Coordinate(0,0)]!!
        val end = blocks[Coordinate(input.first().length - 1, input.size - 1)]!!
        val shortestPath = djikstra(start, end, 1, 3)
        return shortestPath.totalHeatLoss().toString()
    }

    override fun part2(input: List<String>): String {
        val blocks = parseInput(input)
        val start = blocks[Coordinate(0,0)]!!
        val end = blocks[Coordinate(input.first().length - 1, input.size - 1)]!!
        val shortestPath = djikstra(start, end, 4, 10)
        return shortestPath.totalHeatLoss().toString()
    }

}

private val heatLossMemo = Memo<List<Block>, Int>()
private fun List<Block>.totalHeatLoss() : Int = heatLossMemo.memoize(this) {
    if(size <= 1)  {
        0
    } else {
        last().heatLoss + dropLast(1).totalHeatLoss()

    }
}

private data class Block(val heatLoss: Int, val coordinate: Coordinate) {
    val neighbours: MutableList<Block> = mutableListOf()
}