import java.io.File
import kotlin.math.acos
import kotlin.math.atan2
import kotlin.math.pow
import kotlin.math.sqrt

fun main() {
    val input = File("in/day10.txt").readLines()
    //val input = listOf(
    //    "......#.#.",
    //    "#..#.#....",
    //    "..#######.",
    //    ".#.#.###..",
    //    ".#..#.....",
    //    "..#....#.#",
    //    "#..#....#.",
    //    ".##.#..###",
    //    "##...#..#.",
    //    ".#....####"
    //)

    val coordMap = input.mapIndexed { y, row -> row.mapIndexedNotNull { x, c -> if (c == '#') listOf(x, y).map { it.toDouble() } else null } }
    val coords = coordMap.flatten()
    val asteroids = coords.map { Asteroid(it) }
    val asteroidViewCount = asteroids.map { it.detectAsteroids(coords) }
    println("Most detected: ${asteroidViewCount.max()}")
    println(angleBetween(listOf(0.0,0.0), listOf(0.0, -1.0)))

}



fun angleBetween(a: List<Double>, b: List<Double>): Double {
    val (x, y)  = vecMinus(b, a)
    return atan2(y, x)
}

fun dotProduct(a: List<Double>, b: List<Double>) = a.zip(b).fold(0.0, { acc, (u, v)-> acc + u * v })

fun magnitude(a: List<Double>) = sqrt(a.fold(0.0, {acc, i -> acc + i.toDouble().pow(2)  }))

fun normalize(a: List<Double>): List<Double> {
    val len = magnitude(a)
    return a.map { it / len }
}

fun vecMinus(a: List<Double>, b: List<Double>) : List<Double> {
    return a.zip(b).map {it.first - it.second}
}

class Asteroid(val position: List<Double>) {
    fun detectAsteroids(asteroidMap: List<List<Double>>) =
        asteroidMap
            .filter { it != position }
            .map { angleBetween(it, position) }
            .toSet()
            .size
}