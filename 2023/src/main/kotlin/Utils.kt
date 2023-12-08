import kotlin.math.abs
import kotlin.math.sqrt

/**
 * E.g.
 * cartesianProduct(listOf(1, 2, 3), listOf(true, false)) returns
 *  [(1, true), (1, false), (2, true), (2, false), (3, true), (3, false)]
 */
fun <T, U> Collection<T>.cartesianProduct(other: Collection<U>): List<Pair<T, U>> {
    return this.flatMap { lhsElem -> other.map { rhsElem -> lhsElem to rhsElem } }
}

fun manhattanDistance(pos1 : Pair<Int, Int>, pos2 : Pair<Int, Int>): Int {
    return abs(pos1.first - pos2.first) + abs(pos1.second - pos2.second)
}

fun <T> List<T>.split(delimiter: T): List<List<T>> {
    val result = mutableListOf<List<T>>()
    var currentList = mutableListOf<T>()
    result.add(currentList)

    for (element in this) {
        if (element == delimiter) {
            currentList = mutableListOf()
            result.add(currentList)
        } else {
            currentList.add(element)
        }
    }

    return result.toList()
}

fun <T> List<T>.toPair(): Pair<T, T> {
    if (this.size != 2) {
        throw IllegalArgumentException("List is not of length 2!")
    }
    return Pair(this[0], this[1])
}

fun generatePointsBetween(from: Pair<Int, Int>, to: Pair<Int, Int>) : List<Pair<Int, Int>> {
    val x = (if(from.first < to.first) from.first..to.first else from.first downTo to.first).toList()
    val y = (if(from.second < to.second) from.second..to.second else from.second downTo to.second).toList()

    if(x.size < y.size) {
        return y.map { Pair(x[0], it) }
    } else if (x.size > y.size) {
        return x.map { Pair(it, y[0]) }
    }

    return x.zip(y)
}

fun String.sorted(): String {
    return this.toCharArray().sorted().joinToString("")
}

fun <T> List<List<T>>.isValidPoint(point: Pair<Int, Int>) : Boolean =
    point.second in this.indices && point.first in this[point.second].indices

fun <T> replicate(n: Int, x: T): List<T> {
    return List(n) { x }
}

fun <T> replicate(n: Int, f: (Int) -> T): List<T> {
    return List(n) { f(it) }
}

fun leastCommonMultiple(a: Long, b: Long): Long {
    return a * b / greatestCommonDivisor(a, b)
}

fun greatestCommonDivisor(a: Long, b: Long): Long {
    return if (b == 0L) a else greatestCommonDivisor(b, a % b)
}

fun solveQuadraticEq(a: Double, b: Double, c: Double): Pair<Double, Double> {
    val sqrt = sqrt(b * b - 4 * a * c)
    val x1 = (-b + sqrt) / (2 * a)
    val x2 = (-b - sqrt) / (2 * a)
    return Pair(x1, x2)
}

data class Coordinate(val x: Int, val y: Int)