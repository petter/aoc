
import kotlin.math.pow

class Day12 : Day {

    private fun parseInput(input: List<String>): List<Pair<String, List<Int>>> {
        return input.map { line ->
            val split = line.split(" ")
            val controlNumbers = split[1].split(",").map { it.toInt() }
            Pair(split[0], controlNumbers)
        }
    }

    private fun binaryPermutations(length: Int): Sequence<String> = if(length == 0) emptySequence() else sequence {
        for (i in 0 until 2.0.pow(length.toDouble()).toLong()) {
            yield(i.toString(2).padStart(length, '0'))
        }
    }

    private fun printWithDepth(line: String, depth: Int) {
//        println("  ".repeat(depth) + line)
    }

    private val findVariationsMemo = Memo<Pair<String, List<Int>>, Long>()
    private fun findVariations(line: String, controlNumbers: List<Int>, depth: Int = 0) : Long = findVariationsMemo.memoize(line to controlNumbers) {
        fun print(s: String) = printWithDepth(s, depth)
        print("Line: $line, controlNumbers: $controlNumbers")
        if(controlNumbers.isEmpty()) {
            val brokenSprings = line.count { it == '#' }
            return@memoize if (brokenSprings > 0) {
                print("<- 0: Control numbers was empty, but still broken springs left")
                0L
            } else {
                print("<- 1: Control numbers was empty, and no broken springs left!")
                1L
            }
        }

        if(line.isEmpty()) {
            print("<- 0: Line empty and still control numbers left")
            return@memoize 0L
        }

        if(line.length < controlNumbers.sum() + controlNumbers.size - 1) {
            print("<- 0: Line too short to adhere to control numbers")
            return@memoize 0L
        }

        val head = controlNumbers.first()
        val tail = controlNumbers.drop(1)

        var segment = ""
        var i = 0
        while (i < line.length) {
            when (line[i++]) {
                '#' -> segment += '#'
                '?' -> {
                    val restOfLine = line.substring(segment.length + 1)
                    print("question mark found, branching")
                    return@memoize listOf(
                        findVariations("$segment.$restOfLine", controlNumbers, depth + 1),
                        findVariations("$segment#$restOfLine", controlNumbers, depth + 1),
                    ).sum()
                }
                '.' -> {
                    segment += '.'
                    if(segment.contains("#".repeat(head))) {
                        val nextLine = line.substringAfter(segment)
                        print("Segment $segment adheres to control number $head, starting new check with line: $nextLine")
                        return@memoize findVariations(
                            line.substringAfter(segment),
                            tail, depth + 1
                        )
                    }
                }
            }

            val restOfLine = line.substring(i)
            if(restOfLine.length < tail.sum() + tail.size - 1) {
                print("<- 0: Rest of line too short to adhere to control numbers")
                return@memoize 0L
            }


            if (segment.count { it == '#' } > head) {
                print("<- 0: Segment $segment has more broken springs than control number $head")
                return@memoize 0L
            }

            if (segment.contains("#.#")) {
                print("<- 0: segment is split up into two parts, will not be able to adhere to control number $head")
                return@memoize 0L
            }
        }

        if(segment.count { it == '#' } == head && segment.contains("#".repeat(head))) {
            val nextLine = line.substringAfter(segment)
            print("Segment $segment adheres to control number $head, starting new check with line: $nextLine")
            return@memoize findVariations(
                line.substringAfter(segment),
                tail, depth + 1
            )
        }

        0L
    }

    override fun part1(input: List<String>): String {
//        println("\n\n\n-------")
        val parsedInput = parseInput(input)
        val (line, numbers) = parsedInput[0]
//        return findVariations(line, numbers).toString()
//        return parsedInput.map { (line, controlNumbers) ->
//            findVariations(line, controlNumbers)
//        }.toString()
        return parsedInput.sumOf { (line, controlNumbers) ->
            findVariations(line, controlNumbers)
        }.toString()
    }

    override fun part2(input: List<String>): String {
//        return ""
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
//        val (line, controlNumbers) = parsedInput2[1]
//        return findVariations(line, controlNumbers).toString()
//        return parsedInput2.map { (line, controlNumbers) ->
//            findVariations(line, controlNumbers)
//        }.toString()
        var sum = 0L
        for ((index, inp) in parsedInput2.withIndex()) {
            println("Starting $index")
            sum += findVariations(inp.first, inp.second)
            println("Finish $index")
        }
        return sum.toString()
    }

}