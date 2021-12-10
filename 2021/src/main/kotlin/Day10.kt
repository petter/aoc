val openingBrackets = listOf('<', '{', '[', '(')
val closingBrackets = listOf('>', '}', ']', ')')
val matchingBracket = openingBrackets.zip(closingBrackets).toMap()
val syntaxErrorScore = mapOf(
    ')' to 3,
    ']' to 57,
    '}' to 1197,
    '>' to 25137
)
val completionScore = mapOf(
    ')' to 1,
    ']' to 2,
    '}' to 3,
    '>' to 4
)

class Day10 : Day {

    private fun parseInput(input: List<String>): Pair<List<List<Chunk>>, List<List<Chunk>>> {
        val chunks = mutableListOf<List<Chunk>>()
        for (line in input) {
            var curChunk : Chunk? = null
            val lineChunks = mutableListOf<Chunk>()
            lineLoop@ for (char in line) {
                if(char in openingBrackets) {
                    curChunk = Chunk(char, curChunk)
                    lineChunks.add(curChunk)
                } else {
                    curChunk?.close(char)
                    curChunk = curChunk?.parent
                }
            }
            chunks.add(lineChunks)
        }

        return chunks.partition { line -> line.any { it.hasIncorrectClosingBracket() } }
    }

    override fun part1(input: List<String>): String {
        val (syntaxErrorLines, _) = parseInput(input)
        return syntaxErrorLines
            .mapNotNull { lineChunks ->  lineChunks.find { it.hasIncorrectClosingBracket() } }
            .sumOf { it.syntaxErrorScore() }
            .toString()
    }

    // 5188410542 - too high
    override fun part2(input: List<String>): String {
        val (dab, incompleteLines) = parseInput(input)
        val scores = incompleteLines.map { completeLine(it) }.sorted()
        return scores[(scores.size - 1) / 2].toString()
    }

    private fun completeLine(lineChunks: List<Chunk>): Long {
        var lineScore = 0L
        for (chunkIndex in (lineChunks.size - 1) downTo 0) {
            val chunk = lineChunks[chunkIndex]
            if (chunk.isOpen()) {
                lineScore *= 5L
                lineScore += chunk.complete()
            }
        }
        return lineScore
    }
}

class Chunk(private val startBracket: Char, val parent: Chunk? = null) {
    init {
        parent?.addChild(this)
    }
    private val children : MutableList<Chunk> = mutableListOf()
    private var closingBracket : Char? = null

    fun close(closingBracket: Char) {
        this.closingBracket = closingBracket
    }

    fun isOpen() : Boolean = closingBracket == null

    fun hasIncorrectClosingBracket(): Boolean =
        closingBracket != null && closingBracket != matchingBracket[startBracket]

    fun syntaxErrorScore() : Int = if(hasIncorrectClosingBracket()) syntaxErrorScore[closingBracket]!! else 0

    fun complete() : Int {
        val closeBracket = matchingBracket[startBracket] ?: throw IllegalStateException("No matching bracket for $startBracket")
        close(closeBracket)
        return completionScore[closeBracket] ?: throw IllegalStateException("No completion score for $closeBracket")
    }

    private fun addChild(child: Chunk) {
        children.add(child)
    }
}

