/**
 * E.g.
 * cartesianProduct(listOf(1, 2, 3), listOf(true, false)) returns
 *  [(1, true), (1, false), (2, true), (2, false), (3, true), (3, false)]
 */
fun <T, U> Collection<T>.cartesianProduct(other: Collection<U>): List<Pair<T, U>> {
    return this.flatMap { lhsElem -> other.map { rhsElem -> lhsElem to rhsElem } }
}

fun manhattanDistance(pos1 : Pair<Int, Int>, pos2 : Pair<Int, Int>): Int {
    return Math.abs(pos1.first - pos2.first) + Math.abs(pos1.second - pos2.second)
}

fun listSplit(list: List<String>, delimiter: String): List<List<String>> {
    val result = mutableListOf<List<String>>()
    var currentList = mutableListOf<String>()
    result.add(currentList)
    for (element in list) {
        if (element == delimiter) {
            currentList = mutableListOf()
            result.add(currentList)
        } else {
            currentList.add(element)
        }
    }
    return result.toList().filter { it.isNotEmpty() }
}
