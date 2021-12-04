class Day4 : Day{
    private fun parseInput(input: List<String>): Pair<List<Int>, List<BingoCard>> {
        val splitList = listSplit(input, "")
        val drawnNumbers = splitList.first().first().split(",").map { it.toInt() }
        val bingoCards = splitList.drop(1)
            .map { card -> card.map { row -> row.trim().split("\\s+".toRegex()).map { it.toInt() } } }
            .map { BingoCard(it) }

        return drawnNumbers to bingoCards
    }

    override fun part1(input: List<String>): String {
        val (drawnNumbers, bingoCards) = parseInput(input)

        val number = drawnNumbers.find { bingoCards.any { bingoCard -> bingoCard.mark(it) } }
        val bingoCard = bingoCards.find { it.isBingo() }

        return if (number != null && bingoCard != null) {
            (number * bingoCard.sumUnmarked()).toString()
        } else {
            "Error, no bingo"
        }
    }

    override fun part2(input: List<String>): String {
        val (drawnNumbers, _bingoCards) = parseInput(input)
        var bingoCards = _bingoCards
        for (number in drawnNumbers) {
            bingoCards.forEach { it.mark(number) }
            val unfilteredBingoCards = bingoCards
            bingoCards = bingoCards.filter { !it.isBingo() }
            if(bingoCards.isEmpty()) {
                return (unfilteredBingoCards.first().sumUnmarked() * number).toString()
            }
        }
        return ""
    }
}

class BingoCard(input: List<List<Int>>) {
    val rows = input.map { row -> row.map { BingoCardCell(it) } }

    fun mark(number: Int): Boolean {
        rows.forEach { row -> row.forEach { cell -> if(cell.number == number) cell.draw() } }
        return isBingo()
    }

    fun isBingo(): Boolean {
        if (rows.any { row -> row.all { cell -> cell.isMarked() } }) return true
        for (column in rows.indices) {
            if (rows.all { row -> row[column].isMarked() }) return true
        }
        return false
    }

    fun sumUnmarked(): Int {
        return rows.flatMap { row -> row.map { cell -> if(cell.isMarked()) 0 else cell.number } }.sum()
    }

    class BingoCardCell(val number: Int) {
        private var marked = false
        fun draw(): Boolean {
            marked = true
            return marked
        }

        fun isMarked() = marked
    }
}