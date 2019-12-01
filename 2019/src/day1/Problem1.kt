package day1

import java.io.File

fun main() {
    val sum = File("in/day1.txt").readLines().map { calculateFuel(it.toDouble()) }.sum()
    println("Sum of modules: ${sum}")
}

fun calculateFuel(mass: Double) = (mass / 3).toInt() - 2