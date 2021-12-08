class Day8 : Day {
    override fun part1(input: List<String>): String {
        return input.sumOf { line ->
            val output = line.split(" | ")[1]
            output.split(" ").filter { it.length in listOf(2, 3, 4, 7) }.size
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return input.sumOf { line ->
            val (lineInput, lineOutput) = line.split(" | ").map { it.split(" ").map { segment -> segment.sorted() }  }
            val parser = SevenSegmentParser(lineInput)
            lineOutput.joinToString("") { parser.parse(it).toString() }.toInt()
        }.toString()
    }
}

/*
 * two segments - 1
 * three segments - 7
 * four segments - 4
 * five segments - 2, 3, 5
 * six segements - 0, 6, 9
 * seven segments - 8
 */
class SevenSegmentParser(trainingSet : List<String>) {
    private val segmentsToNum = mutableMapOf<String, Int>()
    init {
        val one = trainingSet.find { it.length == 2 }!!
        val four = trainingSet.find { it.length == 4 }!!
        val seven = trainingSet.find { it.length == 3 }!!
        val eight = trainingSet.find { it.length == 7 }!!

        val middleAndTopLeftSegment = four.filter { it !in one }
        val sixSegmentNumbers = trainingSet.filter { it.length == 6 }
        val sixOrNine = sixSegmentNumbers.filter { middleAndTopLeftSegment.all { segment -> segment in it } }
        val zero = sixSegmentNumbers.find { it !in sixOrNine }!!
        val (six, nine) = if(one.any { it !in sixOrNine.first() }) {
            Pair(sixOrNine.first(), sixOrNine.last())
        } else {
            Pair(sixOrNine.last(), sixOrNine.first())
        }
        val middleSegment = eight.find { it !in zero }!!
        val topLeftSegment = middleAndTopLeftSegment.find { it != middleSegment }!!
        val three = nine.filter { it != topLeftSegment }
        val fiveSegmentNumbers = trainingSet.filter { it.length == 5 }
        val twoOrFive = fiveSegmentNumbers.filter { it != three }.toPair()
        val (two, five) = if(topLeftSegment !in twoOrFive.first) twoOrFive else twoOrFive.second to twoOrFive.first

        segmentsToNum[zero] = 0
        segmentsToNum[one] = 1
        segmentsToNum[two] = 2
        segmentsToNum[three] = 3
        segmentsToNum[four] = 4
        segmentsToNum[five] = 5
        segmentsToNum[six] = 6
        segmentsToNum[seven] = 7
        segmentsToNum[eight] = 8
        segmentsToNum[nine] = 9
    }

    fun parse(segments: String) : Int {
        val key = segments.sorted()
        return segmentsToNum[key] ?: throw Error("No mapping for $key")
    }
}