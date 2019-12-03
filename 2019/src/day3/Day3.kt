package day3

import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val grid = WireGrid()
    File("in/day3.txt").readLines().forEach { grid.addWire(it) }
    println("Closest distance to intersection: ${grid.closestDistanceToIntersection()}")
    println("Minimum steps intersections: ${grid.minimumStepsIntersection()}")
}

class WireGrid() {
    val map = HashMap<String, Array<String>>()
    val intersections = mutableListOf<Array<Int>>()
    val intersectionSteps = mutableListOf<Array<Int>>()

    companion object {
        var curId = 0
        fun getNewId() = curId++
    }

    fun addWire(path: String) {
        val wireId = getNewId().toString()
        var steps = 0

        val moves = path.split(",")
        var coords = arrayOf(0,0)
        for (move in moves) {
            val dir = move[0]
            val amount = move.substring(1).toInt()
            for(i in 1..amount) {
                steps++
                when(dir) {
                    'U' -> coords[1] += 1
                    'D' -> coords[1] -= 1
                    'L' -> coords[0] -= 1
                    'R' -> coords[0] += 1
                }

                val key = coords.joinToString(",")
                val mapPos = map[key]
                if(mapPos != null && mapPos[0] != wireId && mapPos[0] != "X") {
                    map[key] = arrayOf("X", "X")
                    intersections.add(coords.clone())
                    intersectionSteps.add(arrayOf(mapPos[1].toInt(), steps))
                } else if(mapPos == null) {
                    map[key] = arrayOf(wireId, steps.toString())
                }
            }
        }

    }

    fun closestDistanceToIntersection() = intersections.map { it.map { it.absoluteValue }.sum() }.min()

    fun minimumStepsIntersection() = intersectionSteps.map { it.sum() }.min()
}

