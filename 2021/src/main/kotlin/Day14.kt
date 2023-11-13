import java.math.BigInteger

class Day14 : Day {
    private var rules: Map<Pair<Char, Char>, Char> = emptyMap()

    private fun parseInput(input: List<String>): String {
        val polymerTemplate = input.first()
        this.rules = input.drop(2).map {
            val (key, value) = it.split(" -> ")
            key.toCharArray().toList().toPair() to value.first()
        }.let { it.associate { id -> id } }

        return polymerTemplate
    }

    private fun step(
        charPair: Pair<Char, Char>,
        depth: Int
    ): Map<Char, BigInteger> {
        if(depth == 0) {
            return emptyMap()
        }

        val newChar = rules[charPair] ?: throw Error("Ingen regler for $charPair")
        val res = mapOf<Char, BigInteger>(newChar to BigInteger.ONE)

        val (char1, char2) = charPair
        val mapA = memoizedStep(char1 to newChar, depth - 1)
        val mapB = memoizedStep(newChar to char2, depth - 1)

        return mergeMaps(res, mergeMaps(mapA, mapB))
    }

    private val cache = mutableMapOf<Pair<Pair<Char, Char>, Int>, Map<Char, BigInteger>>()
    private fun memoizedStep(charPair: Pair<Char, Char>, depth: Int): Map<Char, BigInteger> {
        val cacheKey = charPair to depth
        return cache.getOrPut(cacheKey) {
            step(charPair, depth)
        }
    }

    private fun mergeMaps(mapA: Map<Char, BigInteger>, mapB: Map<Char, BigInteger>) : Map<Char, BigInteger> {
        val res = mapA.toMutableMap()
        mapB.entries.forEach {
            res[it.key] = res.getOrDefault(it.key, BigInteger.ZERO) + it.value
        }
        return res
    }

    override fun part1(input: List<String>): String {
        println("Part 1:")
        val polymer = parseInput(input)
        val polymerCharCounts = polymer.groupBy { it }.entries.associate { it.key to it.value.size.toBigInteger() }

        var i = 0
        val polymerPairs = polymer.zipWithNext()
        val charCounts = polymerPairs.map {
            val res = memoizedStep(it, 10)
            println("${++i}/${polymerPairs.size}")
            res
        }
        val mergedCharCount = mergeMaps(polymerCharCounts, charCounts.fold(emptyMap()) { acc, map -> mergeMaps(acc, map) })

        val mostCommonCount = mergedCharCount.entries.maxByOrNull { it.value }?.value ?: throw Error("No most common char")
        val leastCommonCount = mergedCharCount.entries.minByOrNull { it.value }?.value ?: throw Error("No least common char")

        return (mostCommonCount - leastCommonCount).toString()
    }

    override fun part2(input: List<String>): String {
        println("Part 2:")
        val polymer = parseInput(input)
        val polymerCharCounts = polymer.groupBy { it }.entries.associate { it.key to it.value.size.toBigInteger() }

        var i = 0
        val polymerPairs = polymer.zipWithNext()
        val charCounts = polymerPairs.map {
            val res = memoizedStep(it, 40)
            println("${++i}/${polymerPairs.size}")
            res
        }
        val mergedCharCount = mergeMaps(polymerCharCounts, charCounts.fold(emptyMap()) { acc, map -> mergeMaps(acc, map) })

        val mostCommonCount = mergedCharCount.entries.maxByOrNull { it.value }?.value ?: throw Error("No most common char")
        val leastCommonCount = mergedCharCount.entries.minByOrNull { it.value }?.value ?: throw Error("No least common char")

        return (mostCommonCount - leastCommonCount).toString()
    }
}

