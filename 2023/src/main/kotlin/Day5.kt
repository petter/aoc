import java.math.BigInteger

class Day5 : Day {

    // Returns a pair of seeds to use, and a list of transformers
    private fun parseInput(input: List<String>): Pair<List<Long>, List<Transformer>> {
        val seeds = input
            .first()
            .substringAfter("seeds: ")
            .split(" ")
            .map { it.toLong() }

        val transformers = input
            .drop(2)
            .split("")
            .map { group ->
                val rules = group.drop(1).map { ranges ->
                    val (destinationStart, sourceStart, rangeLength) = ranges
                        .split(" ")
                        .map { it.toLong() }
                    Rule(Triple(destinationStart, sourceStart, rangeLength))
                }
                Transformer(rules)
            }

        return seeds to transformers
    }

    override fun part1(input: List<String>): String {
        val (seeds, transformers) = parseInput(input)
        val locations = seeds.map { seed ->
            transformers.fold(seed) { x, transformer -> transformer.transform(x) }
        }
        return locations.min().toString()
    }

    // 2520480 - too high
    override fun part2(input: List<String>): String {
        val (seedsRangePairs, transformers) = parseInput(input)
        val seedRanges = seedsRangePairs.chunked(2).map { (start, length) -> (start..(start + length)) }
        val locations = seedRanges.mapIndexed { i, seedRange ->
            println("Running seed range $i/${seedRanges.size} with ${seedRange.endInclusive - seedRange.start} seeds")
            seedRange.minOf { seed -> transformers.fold(seed) { x, transformer -> transformer.transform(x) } }
        }
        return locations.min().toString()
    }

}

private class Rule(destinationSourceRange: Triple<Long, Long, Long>) {
    private val destinationStart = destinationSourceRange.first
    private val sourceStart = destinationSourceRange.second
    private val rangeLength = destinationSourceRange.third

    private val sourceEnd = sourceStart + rangeLength - 1

    fun apply(source: Long): Long? {
        if(source in sourceStart..sourceEnd) {
            return destinationStart + source - sourceStart
        }

        return null
    }
}

private class Transformer(private val rules: List<Rule>) {
    fun transform(source: Long): Long {
        for(rule in rules) {
            val result = rule.apply(source)
            if(result != null) {
                return result
            }
        }

        return source
    }
}