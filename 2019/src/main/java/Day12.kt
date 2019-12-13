package day12

import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val moons = File("in/day12.txt").readLines().mapNotNull { line ->
        val groups = Regex("^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>$").find(line)?.groupValues ?: return@mapNotNull null
        val vec = Vector3(groups[1].toInt(), groups[2].toInt(), groups[3].toInt())
        Moon(vec)
    }
    val initialState = moons.map { it.getPosVel().toList().map(Vector3::toList).flatten()  }

    var step = 0
    val seq = generateSequence {
        step++
        moons.forEach { it.applyGravity(moons) }
        moons.forEach(Moon::applyVelocity)
        moons
            .map { moon -> moon.getPosVel() }
            .map { (a, b) -> a.toList().zip(b.toList()) }
            .withIndex().flatMap { (moonNumber, moon) -> moon.mapIndexed { axisIndex, (pos, vel) -> StepState(step, moonNumber, axisIndex, pos.toInt(), vel.toInt()) } }
    }

    val res = seq
        .flatMap { it.asSequence() }
        .filter { initialState[it.moonNumber][it.axisNumber].toInt() == it.pos && initialState[it.moonNumber][it.axisNumber + 3].toInt() == it.vel}

    val test = initialState.map { it.map { mutableListOf<Int>() } }
    val patterns = mutableMapOf<Pair<Int, Int>, Int>()
    val maxPatternSize = initialState.size * initialState[0].size
    res.forEach{ state ->
        val list = test[state.moonNumber][state.axisNumber]
        list.add(state.step)
        val pattern = list
            .find { list.contains(it * 2) }
            //.flatMap { a -> list.map { b -> Pair(a, b)} }
            //.filter { (a, b) -> a > b }
            //.map { (a, b) -> a - b }
            //.groupBy { it }
            //.filterValues { list.conta}
            //.mapValues { it.value.size }.maxBy { it.value }?.key
        if (pattern != null) {
            patterns[Pair(state.moonNumber, state.axisNumber)] = pattern
            val patternSet = patterns.map { it.value }.toSet()
            println("Found ${patterns.size} - ${patternSet}")
            if (patterns.size >= maxPatternSize) {
                val a = patternSet.fold(1L, { acc, i ->  lcm(acc, i.toLong())})
                println("$a steps until history repeats itself")
                return
            }
        }
    }

}

data class StepState(val step: Int, val moonNumber: Int, val axisNumber: Int, val pos: Int, val vel: Int)

data class Moon(var position: Vector3) {
    var velocity = Vector3(0, 0, 0)

    fun applyVelocity() {
        position += velocity
    }

    fun applyGravity(others: List<Moon>) {
        velocity += others.map{ (it.position - position).normalizeAxis() }.sum()
    }

    fun calculateEnergy() = position.sumAbsoluteAxis() * velocity.sumAbsoluteAxis()

    fun getPosVel() = Pair(position, velocity)


    override fun toString(): String = "pos=$position, vel=$velocity"
}

fun List<Vector3>.sum() = this.reduce {acc, vector3 -> acc + vector3}

data class Vector3(val x: Int, val y: Int, val z: Int) {
    operator fun plus(other: Vector3) = Vector3(x + other.x, y + other.y, z + other.z)
    operator fun minus(other: Vector3) = Vector3(x - other.x, y - other.y, z - other.z)
    operator fun times(number: Int) = Vector3(x * number, y * number, z * number)
    private fun normalizeSingleAxis(number: Int) = if (number > 0) 1 else if (number < 0) -1 else 0
    fun toArray() = arrayOf(x, y, z)
    fun toList() = listOf(x.toLong(), y.toLong(), z.toLong())
    fun normalizeAxis() = Vector3(normalizeSingleAxis(x), normalizeSingleAxis(y), normalizeSingleAxis(z))
    fun sumAbsoluteAxis() = x.absoluteValue + y.absoluteValue + z.absoluteValue
}

fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)
fun lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

//          882565200 too low
//         4130781092 too low
//        13826854800 too low
//        74014340400 too low
//      3176570659748 not right
//    865295369246700 not right
// 665412138950712300 not right