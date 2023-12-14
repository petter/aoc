

class Day12 : Day {

    private fun parseInput(input: List<String>): List<Pair<String, List<Int>>> {
        return input.map { line ->
            val split = line.split(" ")
            val controlNumbers = split[1].split(",").map { it.toInt() }
            Pair(split[0], controlNumbers)
        }
    }

    private val findVariationsMemo = Memo<Pair<String, List<Int>>, Long>()
    private fun findVariations(line: String, controlNumbers: List<Int>) : Long = findVariationsMemo.memoize(line to controlNumbers) {
        if(controlNumbers.isEmpty()) {
            val brokenSprings = line.count { it == '#' }
            return@memoize if (brokenSprings > 0) 0L else 1L
        }

        if(line.isEmpty()) return@memoize 0L

        if(line.length < controlNumbers.sum() + controlNumbers.size - 1) return@memoize 0L

        val head = controlNumbers.first()
        val tail = controlNumbers.drop(1)

        var segment = ""
        var i = 0
        while (i < line.length) {
            when (line[i++]) {
                '#' -> segment += '#'
                '?' -> {
                    val restOfLine = line.substring(segment.length + 1)
                    return@memoize listOf(
                        findVariations("$segment.$restOfLine", controlNumbers),
                        findVariations("$segment#$restOfLine", controlNumbers),
                    ).sum()
                }
                '.' -> {
                    segment += '.'
                    val possibleMatch = segment.dropWhile { it == '.' }.takeWhile { it == '#' }
                    if(possibleMatch.length == head) {
                        return@memoize findVariations(line.substringAfter(segment), tail)
                    } else if(possibleMatch.isNotEmpty()) {
                        return@memoize 0L
                    }
                }
            }

            val restOfLine = line.substring(i)
            if(restOfLine.length < tail.sum() + tail.size - 1) {
                return@memoize 0L
            }


            if (segment.count { it == '#' } > head) {
                return@memoize 0L
            }

            if (segment.contains("#.#")) {
                return@memoize 0L
            }
        }

        if(segment.count { it == '#' } == head && segment.contains("#".repeat(head))) {
            return@memoize findVariations(line.substringAfter(segment), tail)
        }

        0L
    }

    override fun part1(input: List<String>): String {
        val parsedInput = parseInput(input)
        return parsedInput.sumOf { (line, controlNumbers) ->
            findVariations(line, controlNumbers)
        }.toString()
    }

    override fun part2(input: List<String>): String {
        val parsedInput = parseInput(input)
        val parsedInput2 = parsedInput.map { (line, controlNumbers) ->
            listOf(line, line, line, line, line).joinToString("?") to listOf(
                controlNumbers,
                controlNumbers,
                controlNumbers,
                controlNumbers,
                controlNumbers
            ).flatten()
        }
        return parsedInput2.sumOf { (line, controlNumbers) ->
            findVariations(line, controlNumbers)
        }.toString()
    }
}
