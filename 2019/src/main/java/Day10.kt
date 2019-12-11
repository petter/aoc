import java.io.File
import kotlin.math.*

fun main() {
    val input = File("in/day10.txt").readLines()
    val coordMap = input.mapIndexed { y, row -> row.mapIndexedNotNull { x, c -> if (c == '#') listOf(x, y).map { it.toDouble() } else null } }
    val coords = coordMap.flatten()
    val asteroids = coords.map { Asteroid(it) }
    val asteroidViewCount = asteroids.map { it.detectAsteroids(coords) }
    val maxAsteroidCount = asteroidViewCount.max()
    val asteroidMaxCount = asteroids[asteroidViewCount.indexOf(maxAsteroidCount)]
    val shootOrder = asteroidMaxCount.asteroidShootOrder(coords)
    println("Most detected $maxAsteroidCount at position ${asteroidMaxCount.position}")
    println("The 200th shot asteroid is: ${shootOrder[199]}")
    println(shootOrder)
}

fun angleBetween(a: List<Double>, b: List<Double>): Double {
    val (x, y)  = vecMinus(b, a)
    return atan2(x, -y)
}

fun magnitude(a: List<Double>) = sqrt(a.fold(0.0, {acc, i -> acc + i.pow(2)  }))

fun vecMinus(a: List<Double>, b: List<Double>) : List<Double> {
    return a.zip(b).map {it.first - it.second}
}

class Asteroid(val position: List<Double>) {
    fun detectAsteroids(asteroidMap: List<List<Double>>) =
        asteroidMap
            .filter { it != position }
            .map { angleBetween(position, it) }
            .toSet()
            .size

    fun asteroidShootOrder(asteroidMap: List<List<Double>>) = asteroidMap
        .asSequence()
        .filter { it != position }
        .map { angleBetween(position, it) to it }
        .groupBy { it.first }
        .map { group ->
            val first = (group.key + 2 * PI) % (2 * PI)
            val second = group.value.map { it.second }.sortedBy { magnitude(vecMinus(it, position)) }[0]
            first to second
        }
        .sortedBy { it.first }
        .map { it.second }
        .toList()
}

