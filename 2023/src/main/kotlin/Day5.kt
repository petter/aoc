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

    override fun part2(input: List<String>): String {
        val (seedsRangePairs, transformers) = parseInput(input)
        val seedRanges = seedsRangePairs.chunked(2).map { (start, length) -> (start..(start + length)) }
        val reversedTransformers = transformers.reversed()
        var location = 0L
        while(true) {
            val seed = reversedTransformers.fold(location) { x, transformer -> transformer.reverseTransform(x) }
            if(seedRanges.any { seed in it }) {
                return location.toString()
            }
            location++
        }
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

    fun reverseApply(destination: Long): Long? {
        if(destination in destinationStart..<destinationStart + rangeLength) {
            return sourceStart + destination - destinationStart
        }

        return null
    }
}

private class Transformer(val rules: List<Rule>) {
    fun transform(source: Long): Long {
        for(rule in rules) {
            val result = rule.apply(source)
            if(result != null) {
                return result
            }
        }

        return source
    }

    fun reverseTransform(destination: Long): Long {
        for(rule in rules) {
            val result = rule.reverseApply(destination)
            if(result != null) {
                return result
            }
        }

        return destination
    }
}