class Day13 : Day {

    private fun parseInput(input: List<String>): List<List<String>> {
        return input.split("")
    }

    /**
     * Returns the index of where the horizontal reflection is.
     * 2 = between 1 and 2
     * Returns null if there are no horizontal reflection lines.
     */
    private fun getHorizontalReflectionIndex(pattern: List<String>, withSmudge: Boolean): Int? {
        for (possibleReflectionIndex in 1 until pattern.size) {
            val before = pattern.take(possibleReflectionIndex)
            val after = pattern.subList(possibleReflectionIndex, pattern.size)

            var i = 0
            var allMatch = true
            var smudgeUsed = false
            while (i < before.size && i < after.size) {
                val curBefore = before[before.size - 1 - i]
                val curAfter = after[i]

                if(curBefore != curAfter) {

                    if(withSmudge) {
                        val numberOfDiffs = curBefore.zip(curAfter).count { it.first != it.second }
                        if(numberOfDiffs == 1) {
                            smudgeUsed = true
                            i++
                            continue
                        }
                    }

                    allMatch = false
                    break
                }
                i++
            }

            if(allMatch && withSmudge && smudgeUsed) {
                return possibleReflectionIndex
            }

            if(allMatch && !withSmudge) {
                return possibleReflectionIndex
            }
        }

        return null
    }

    private fun getVerticalReflectionIndex(pattern: List<String>, withSmudge: Boolean): Int? {
        return getHorizontalReflectionIndex(rotateRight(pattern), withSmudge)
    }

    private fun rotateRight(input: List<String>): List<String> {
        val output = mutableListOf<String>()
        for (i in input[0].indices) {
            val row = input.map { it[i] }.reversed().joinToString("")
            output.add(row)
        }
        return output
    }

    private fun findReflectionValue(pattern: List<String>, withSmudge: Boolean): Int {
        val horizontalReflectionIndex = getHorizontalReflectionIndex(pattern, withSmudge)
        if(horizontalReflectionIndex != null) {
            return 100*horizontalReflectionIndex
        }

        val verticalReflectionIndex = getVerticalReflectionIndex(pattern, withSmudge)
        if(verticalReflectionIndex != null) {
            return verticalReflectionIndex
        }

        throw Error("Could not find any reflection index for pattern $pattern")
    }

    override fun part1(input: List<String>): String {
        val patterns = parseInput(input)
        return patterns.sumOf { findReflectionValue(it, false) }.toString()
    }

    // 28798 - too low
    override fun part2(input: List<String>): String {
        val patterns = parseInput(input)
        return patterns.sumOf { findReflectionValue(it, true) }.toString()
    }

}