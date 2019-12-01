package day1

import java.io.File

fun main() {
    val fuelRequired = File("in/day1.txt").readLines().map { calculateFuel(it.toInt()) }.sum()
    println("Sum of modules: $fuelRequired")
}

fun calculateFuel(mass: Int) : Int {
    val fuel = (mass / 3) - 2
    if(fuel <= 0) return 0;
    return fuel + calculateFuel(fuel)
}