class Day7 : Day {

    private fun parseInputPart1(input: List<String>): List<Hand> {
        return input.map { line ->
            val (cards, bid) = line.split(" ")
            val cardValues = mapOf(
                "A" to 14,
                "K" to 13,
                "Q" to 12,
                "J" to 11,
                "T" to 10
            )
            val cardsParsed = cards.split("").filter { it.isNotBlank() }.map { cardValues.getOrElse(it) { it.toInt() } }
            Hand(cardsParsed, bid.toInt())
        }
    }

    private fun parseInputPart2(input: List<String>): List<Hand> {
        return input.map { line ->
            val (cards, bid) = line.split(" ")
            val cardValues = mapOf(
                "A" to 14,
                "K" to 13,
                "Q" to 12,
                "J" to 0,
                "T" to 10
            )
            val cardsParsed = cards.split("").filter { it.isNotBlank() }.map { cardValues.getOrElse(it) { it.toInt() } }
            Hand(cardsParsed, bid.toInt())
        }
    }

    override fun part1(input: List<String>): String {
        val hands = parseInputPart1(input)

        assert(Hand(listOf(14, 14, 14, 14, 14), 1).handStrength == 6)
        assert(Hand(listOf(14, 14, 14, 14, 11), 1).handStrength == 5)
        assert(Hand(listOf(10, 10, 11, 11, 10), 1).handStrength == 4)
        assert(Hand(listOf(1, 1, 1, 2, 3), 1).handStrength == 3)
        assert(Hand(listOf(1, 1, 2, 2, 3), 1).handStrength == 2)
        assert(Hand(listOf(1, 1, 2, 3, 4), 1).handStrength == 1)
        assert(Hand(listOf(1, 2, 3, 4, 5), 1).handStrength == 0)

        return hands
            .sortedDescending()
            .mapIndexed { index, hand -> hand.bid * (hands.size - index) }
            .sumOf { it.toLong() }
            .toString()
    }

    override fun part2(input: List<String>): String {
        val hands = parseInputPart2(input)

        assert(Hand(listOf(14, 14, 14, 14, 14), 1).handStrength == 6)
        assert(Hand(listOf(14, 0, 14, 14, 14), 1).handStrength == 6)
        assert(Hand(listOf(14, 0, 0, 14, 14), 1).handStrength == 6)
        assert(Hand(listOf(14, 0, 0, 0, 14), 1).handStrength == 6)
        assert(Hand(listOf(0, 0, 0, 0, 0), 1).handStrength == 6)
        assert(Hand(listOf(0, 0, 0, 0, 0), 1) < Hand(listOf(10, 0, 0, 0, 0), 1))
        assert(Hand(listOf(0, 0, 0, 0, 0), 1) > Hand(listOf(10, 11, 0, 0, 0), 1))
        assert(Hand(listOf(10, 11, 0, 0, 0), 1).handStrength == 5)
        assert(Hand(listOf(10, 11, 11, 11, 0), 1).handStrength == 5)
        assert(Hand(listOf(10, 10, 11, 11, 0), 1).handStrength == 4)
        assert(Hand(listOf(10, 10, 11, 11, 10), 1).handStrength == 4)
        assert(Hand(listOf(1, 1, 1, 2, 3), 1).handStrength == 3)
        assert(Hand(listOf(1, 0, 1, 2, 3), 1).handStrength == 3)
        assert(Hand(listOf(1, 0, 0, 2, 3), 1).handStrength == 3)
        assert(Hand(listOf(1, 1, 2, 3, 0), 1).handStrength == 3)
        assert(Hand(listOf(1, 1, 2, 3, 3), 1).handStrength == 2)
        assert(Hand(listOf(1, 0, 2, 3, 4), 1).handStrength == 1)

        return hands
            .sortedDescending()
            .mapIndexed { index, hand -> hand.bid * (hands.size - index) }
            .sumOf { it.toLong() }
            .toString()
    }

}

private data class Hand(val cards: List<Int>, val bid: Int) : Comparable<Hand> {
    private val numberOfJokers = cards.count { it == 0 }
    private val largestGroupWithoutJokers = cards
        .filter { it != 0 }
        .groupBy { it }
        .maxByOrNull { it.value.size }
        ?.value ?: listOf()
    private val pairsWithoutJokers = cards
        .filter { it != 0 }
        .groupBy { it }
        .values.filter { it.size == 2 }.size

    val handStrength = when {
        isFiveOfAKind() -> 6
        isFourOfAKind() -> 5
        isFullHouse() -> 4
        isThreeOfAKind() -> 3
        isTwoPairs() -> 2
        isOnePair() -> 1
        else -> 0
    }

    private fun isFiveOfAKind(): Boolean {
        return largestGroupWithoutJokers.size + numberOfJokers == 5
    }

    private fun isFourOfAKind(): Boolean {
        return largestGroupWithoutJokers.size + numberOfJokers == 4
    }

    private fun isFullHouse(): Boolean {
        val threeOfAKindsWithoutJoker = cards
            .filter { it != 0 }
            .groupBy { it }
            .values.filter { it.size == 3 }.size

        if(threeOfAKindsWithoutJoker == 1 && pairsWithoutJokers == 1) {
            return true
        }

        if(pairsWithoutJokers == 2 && numberOfJokers >= 1) {
            return true
        }

        return false
    }

    private fun isThreeOfAKind(): Boolean {
        return largestGroupWithoutJokers.size + numberOfJokers >= 3
    }

    private fun isTwoPairs(): Boolean {
        return pairsWithoutJokers == 2 || pairsWithoutJokers == 1 && numberOfJokers >= 1 || pairsWithoutJokers == 0 && numberOfJokers >= 2
    }

    private fun isOnePair(): Boolean {
        return largestGroupWithoutJokers.size + numberOfJokers >= 2
    }

    override fun compareTo(other: Hand): Int {
        return when {
            this.handStrength > other.handStrength -> 1
            this.handStrength < other.handStrength -> -1
            else -> {
                for((thisCard, otherCard) in this.cards.zip(other.cards)) {
                    if(thisCard > otherCard) {
                        return 1
                    } else if(thisCard < otherCard) {
                        return -1
                    }
                }

                0
            }
        }
    }
}