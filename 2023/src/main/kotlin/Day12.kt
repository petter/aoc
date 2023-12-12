import kotlin.math.pow

class Day12 : Day {

    private fun parseInput(input: List<String>): List<Pair<String, List<Int>>> {
        return input.map { line ->
            val split = line.split(" ")
            val controlNumbers = split[1].split(",").map { it.toInt() }
            Pair(split[0], controlNumbers)
        }
    }

    private fun binaryPermutations(length: Int): Sequence<String> = sequence {
        for (i in 0 until 2.0.pow(length.toDouble()).toLong()) {
            yield(i.toString(2).padStart(length, '0'))
        }
    }

    private fun findVariations(line: String, controlNumbers: List<Int>): Int {
        val knownBrokenSprings = line.count { it == '#' }
        val brokenSpringsTotal = controlNumbers.sum()
        val questionMarks = line.count { it == '?' }

        val possibleArrangements = binaryPermutations(questionMarks).filter { binaryString ->
            val questionMarksTurningIntoSprings = binaryString.count { it == '1' }
            questionMarksTurningIntoSprings + knownBrokenSprings == brokenSpringsTotal
        }

        var variationMatches = 0
        for (binary in possibleArrangements) {
            var binaryIndex = 0
            val newLine = line.map {
                if(it == '?') {
                    val result = binary[binaryIndex++]
                    if(result == '1') '#' else '.'
                } else {
                    it

                }
            }.joinToString("")

            if(lineAdheresToControlNumbers(newLine, controlNumbers)) {
                variationMatches++
            }
        }

        return variationMatches
    }

    private fun lineAdheresToControlNumbers(line: String, controlNumbers: List<Int>): Boolean {
        if(line.any { it == '?' }) {
            throw Error("Line contains a question mark")
        }

        var groupSize = 0
        var controlGroupIndex = 0
        for (c in line) {
            if(c == '#') {
                groupSize++
            } else {
                if(groupSize > 0) {
                    if(controlGroupIndex >= controlNumbers.size || controlNumbers[controlGroupIndex++] != groupSize) {
                        return false
                    }
                    groupSize = 0
                }
            }
        }

        if(groupSize > 0 && (controlGroupIndex >= controlNumbers.size || controlNumbers[controlGroupIndex] != groupSize)) {
            return false
        }

        return true
    }

    override fun part1(input: List<String>): String {
        val parsedInput = parseInput(input)
        return parsedInput.sumOf { (line, controlNumbers) ->
            findVariations(line, controlNumbers)
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return ""
//        val parsedInput = parseInput(input)
//        val parsedInput2 = parsedInput.map { (line, controlNumbers) ->
//            listOf(line, line, line, line, line).joinToString("?") to listOf(controlNumbers, controlNumbers, controlNumbers, controlNumbers, controlNumbers).flatten()
//         }
//        var i = 0
//        return parsedInput2.sumOf { (line, controlNumbers) ->
//            println("line $i/${parsedInput2.size}")
//            i++
//            findVariations(line, controlNumbers).toLong()
//        }.toString()
    }

}