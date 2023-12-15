class Day15 : Day {
    private fun parseInput(input: List<String>): List<String> {
        return input.first().split(",")
    }

    private fun HASH(s: String) : Long {
        return s.fold(0L) { acc, c -> ((acc + c.code) * 17) % 256 }
    }

    override fun part1(input: List<String>): String {
        val hashGroups = parseInput(input)
        return hashGroups.sumOf { HASH(it) }.toString()
    }

    // 1986152 - too high
    override fun part2(input: List<String>): String {
        val hashGroups = parseInput(input)
        val boxes = (0..255).map { mutableListOf<Pair<String, Int>>() }
        hashGroups.forEach { group ->
            val label = group.takeWhile { it != '-' && it != '=' }
            val boxIndex = HASH(label)
            val box = boxes[boxIndex.toInt()]

            if(group.last() == '-') {
                val indexOfLabel = box.indexOfFirst { it.first == label }
                if(indexOfLabel != -1) {
                    box.removeAt(indexOfLabel)
                }
            } else {
                val focalLength = group.substringAfter('=').toInt()
                val indexOfLabel = box.indexOfFirst { it.first == label }
                if(indexOfLabel != -1) {
                    box[indexOfLabel] = label to focalLength
                } else {
                    box.add(Pair(label, focalLength))
                }
            }
        }

        val focusingPowers = boxes.flatMapIndexed { boxNumber, box ->
            box.mapIndexed { boxSlot, (_, focalLength) -> (boxNumber + 1) * (boxSlot + 1) * focalLength }
        }

        return focusingPowers.sum().toString()
    }

}