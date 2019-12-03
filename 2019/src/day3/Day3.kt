package day3

import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val grid = WireGrid()
    File("in/day3.txt").readLines().forEach { grid.addWire(it) }
    println("Closest distance to intersection: ${grid.closestDistanceToIntersection()}")
}

class WireGrid() {

    val map = HashMap<String, String>()
    val intersections = mutableListOf<Array<Int>>()

    private companion object {
        var curId = 0
        fun getNewId() = curId++
    }

    fun addWire(path: String) {
        val wireId = getNewId()
        val moves = path.split(",")
        var coords = arrayOf(0,0)
        for (move in moves) {
            val dir = move[0]
            val amount = move.substring(1).toInt()
            for(i in 1..amount) {
                when(dir) {
                    'U' -> coords[1] += 1
                    'D' -> coords[1] -= 1
                    'L' -> coords[0] -= 1
                    'R' -> coords[0] += 1
                }

                val key = coords.joinToString(",")
                val mapPos = map[key]
                if(mapPos != null && mapPos != wireId.toString() && mapPos != "X") {
                    map[key] = "X"
                    intersections.add(coords.clone())
                } else {
                    map[key] = wireId.toString()
                }
            }
        }

    }

    fun closestDistanceToIntersection() = intersections.map { it.map { it.absoluteValue }.sum() }.min()

}

