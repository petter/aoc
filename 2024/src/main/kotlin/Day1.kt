import kotlin.math.abs

class Day1 : Day {

    fun parseInput(input: List<String>): Pair<List<Int>, List<Int>> =
        input.map { it.split("   ")
            .map { num -> num.toInt() }.toPair() }
            .unzip()

    override fun part1(input: List<String>): String {
        val (list1, list2) = parseInput(input)
        return totalDistance(list1, list2, 0).toString()
    }

    fun totalDistance(list1: List<Int>, list2: List<Int>, sum: Int): Int {
        if(list1.isEmpty()) return sum

        val smallest1 = list1.minOrNull()!!
        val newList1 = list1.toMutableList()
        newList1.remove(smallest1)

        val smallest2 = list2.minOrNull()!!
        val newList2 = list2.toMutableList()
        newList2.remove(smallest2)

        return totalDistance(newList1, newList2, sum + abs(smallest1 - smallest2))
    }

    override fun part2(input: List<String>): String {
        return ""
    }

}