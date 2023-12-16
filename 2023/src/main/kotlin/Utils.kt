import kotlin.math.abs
import kotlin.math.pow
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

fun manhattanDistance(pos1 : Pair<Long, Long>, pos2 : Pair<Long, Long>): Long {
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

data class Coordinate(val x: Int, val y: Int) {

    companion object {
        val UP = Coordinate(0, 1)
        val DOWN = Coordinate(0, -1)
        val LEFT = Coordinate(-1, 0)
        val RIGHT = Coordinate(1, 0)
    }

    operator fun plus(other: Coordinate): Coordinate {
        return Coordinate(this.x + other.x, this.y + other.y)
    }

    operator fun minus(other: Coordinate): Coordinate {
        return Coordinate(this.x - other.x, this.y - other.y)
    }

    operator fun times(expandBy: Int): Coordinate {
        return Coordinate(this.x * expandBy, this.y * expandBy)
    }

    fun manhattanDistance(other: Coordinate): Int {
        return abs(this.x - other.x) + abs(this.y - other.y)
    }

    fun euclideanDistance(other: Coordinate): Double {
        return sqrt((this.x - other.x).toDouble().pow(2) + (this.y - other.y).toDouble().pow(2))
    }

    fun neighbours(): List<Coordinate> {
        return listOf(
            this + Coordinate(1, 0),
            this + Coordinate(-1, 0),
            this + Coordinate(0, 1),
            this + Coordinate(0, -1),
        )
    }

    fun neighboursWithDiagonals(): List<Coordinate> {
        return listOf(
            this + Coordinate(1, 0),
            this + Coordinate(-1, 0),
            this + Coordinate(0, 1),
            this + Coordinate(0, -1),
            this + Coordinate(1, 1),
            this + Coordinate(-1, 1),
            this + Coordinate(1, -1),
            this + Coordinate(-1, -1),
        )
    }
}

fun <T> List<T>.permutations(): Sequence<List<T>> {
    if (this.isEmpty()) return sequenceOf(emptyList())

    return sequence {
        val head = first()
        val tail = drop(1)

        if (tail.isEmpty()) {
            yield(listOf(head))
        } else {
            for (perm in tail.permutations()) {
                for (i in 0..perm.size) {
                    yield(perm.take(i) + head + perm.drop(i))
                }
            }
        }
    }
}

class Memo<T, R : Any> {
    private val values = mutableMapOf<T, R>()

    fun memoize(cacheKey: T, fn: () -> R): R {
//        return fn()
        return values.getOrPut(cacheKey) { fn() }
    }
}

enum class CardinalDirection {
    North, South, East, West
}

fun CardinalDirection.toCoordinate() : Coordinate {
    return when(this) {
        CardinalDirection.North -> Coordinate(0, -1)
        CardinalDirection.South -> Coordinate(0, 1)
        CardinalDirection.East -> Coordinate(1, 0)
        CardinalDirection.West -> Coordinate(-1, 0)
    }
}