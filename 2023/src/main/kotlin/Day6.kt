import kotlin.math.ceil
import kotlin.math.floor

class Day6 : Day {

    private fun parseInputPart1(input: List<String>): List<Pair<Long, Long>> {
        val lines = input.map { line ->
            line.substringAfter(":")
                .split(" ")
                .filter { it.isNotBlank() }
                .map { it.toLong() }
        }
        val (times, distances) = lines
        return times.zip(distances)
    }

    private fun parseInputPart2(input: List<String>): Pair<Long, Long> {
        val lines = input.map { line ->
            line.substringAfter(":")
                .split(" ")
                .filter { it.isNotBlank() }
                .joinToString("") { it }
                .toLong()
        }
        val (times, distances) = lines
        return times to distances
    }


    private fun calculateDistance(timeHeldButton: Long, totalTime: Long) : Long {
        return timeHeldButton * (totalTime - timeHeldButton)
    }

    private fun numberOfWaysToBeatBruteForce(distanceToBeat: Long, totalTime: Long) : Long {
        return (0..totalTime)
            .asSequence()
            .filter { calculateDistance(it, totalTime) > distanceToBeat }
            .count().toLong()
    }

    private fun numberOfWaysToBeatMath(distanceToBeat: Long, totalTime: Long) : Long {
        val (x1, x2) = solveQuadraticEq(-1.0, totalTime.toDouble(), - distanceToBeat.toDouble())
        return floor(x2).toLong() - floor(x1).toLong()
    }

    override fun part1(input: List<String>): String {
        val races = parseInputPart1(input)
        val numWaysToBeat = races.map { (time, distanceToBeat) ->
            numberOfWaysToBeatMath(distanceToBeat, time)
        }

        return numWaysToBeat.reduce(Long::times).toString()
    }

    override fun part2(input: List<String>): String {
        val (time, distanceToBeat) = parseInputPart2(input)
        return numberOfWaysToBeatMath(distanceToBeat, time).toString()
    }

}