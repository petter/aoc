package day12

import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val moons = File("in/day12.txt").readLines().mapNotNull { line ->
        val groups = Regex("^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>$").find(line)?.groupValues ?: return@mapNotNull null
        val vec = Vector3(groups[1].toInt(), groups[2].toInt(), groups[3].toInt())
        Moon(vec)
    }

    for (i in 0..999) {
        println("\nAfter $i steps")
        moons.forEach(::println)
        moons.forEach { it.applyGravity(moons.filter { other -> other != it }) }
        moons.forEach(Moon::applyVelocity)
    }

    println("Total energy after 1000 steps: ${moons.map(Moon::calculateEnergy).sum()}")
}

data class Moon(var position: Vector3) {
    var velocity = Vector3(0, 0, 0)

    fun applyVelocity() {
        position += velocity
    }

    fun applyGravity(others: List<Moon>) {
        velocity += others.map{ (it.position - position).normalizeAxis() }.sum()
    }

    fun calculateEnergy() = position.sumAbsoluteAxis() * velocity.sumAbsoluteAxis()

    override fun toString(): String = "pos=$position, vel=$velocity"
}

fun List<Vector3>.sum() = this.reduce {acc, vector3 -> acc + vector3}

data class Vector3(val x: Int, val y: Int, val z: Int) {
    operator fun plus(other: Vector3) = Vector3(x + other.x, y + other.y, z + other.z)
    operator fun minus(other: Vector3) = Vector3(x - other.x, y - other.y, z - other.z)
    operator fun times(number: Int) = Vector3(x * number, y * number, z * number)
    private fun normalizeSingleAxis(number: Int) = if (number > 0) 1 else if (number < 0) -1 else 0
    fun normalizeAxis() = Vector3(normalizeSingleAxis(x), normalizeSingleAxis(y), normalizeSingleAxis(z))
    fun sumAbsoluteAxis() = x.absoluteValue + y.absoluteValue + z.absoluteValue
}

// 2082 too low
