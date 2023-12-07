class Day7 : Day {

    private fun parseInput(input: List<String>, cardValues: Map<String, Int>): List<Hand> {
        return input.map { line ->
            val (cards, bid) = line.split(" ")
            val cardsParsed = cards
                .split("")
                .filter { it.isNotBlank() }
                .map { cardValues.getOrElse(it) { it.toInt() } }
            Hand(cardsParsed, bid.toInt())
        }
    }

    override fun part1(input: List<String>): String {
        val cardValues = mapOf(
            "A" to 14,
            "K" to 13,
            "Q" to 12,
            "J" to 11,
            "T" to 10
        )
        val hands = parseInput(input, cardValues)
        return hands
            .sortedDescending()
            .mapIndexed { index, hand -> hand.bid * (hands.size - index) }
            .sumOf { it.toLong() }
            .toString()
    }

    override fun part2(input: List<String>): String {
        val cardValues = mapOf(
            "A" to 14,
            "K" to 13,
            "Q" to 12,
            "J" to 0,
            "T" to 10
        )
        val hands = parseInput(input, cardValues)
        return hands
            .sortedDescending()
            .mapIndexed { index, hand -> hand.bid * (hands.size - index) }
            .sumOf { it.toLong() }
            .toString()
    }

}

private data class Hand(val cards: List<Int>, val bid: Int) : Comparable<Hand> {
    private val groupCounts = cards.groupBy { it }.map { it.value.size }.sortedDescending()
    private val simpleHandType = when(groupCounts.take(2)) {
        listOf(5) -> HandType.FiveOfAKind
        listOf(4, 1) -> HandType.FourOfAKind
        listOf(3, 2) -> HandType.FullHouse
        listOf(3, 1) -> HandType.ThreeOfAKind
        listOf(2, 2) -> HandType.TwoPairs
        listOf(2, 1) -> HandType.OnePair
        else -> HandType.HighCard
    }

    private val numberOfJokers = cards.count { it == 0 }
    val handType = when(simpleHandType to numberOfJokers) {
        HandType.FourOfAKind to 1 -> HandType.FiveOfAKind
        HandType.FourOfAKind to 4 -> HandType.FiveOfAKind
        HandType.FullHouse to 2 -> HandType.FiveOfAKind
        HandType.FullHouse to 3 -> HandType.FiveOfAKind
        HandType.ThreeOfAKind to 1 -> HandType.FourOfAKind
        HandType.ThreeOfAKind to 3 -> HandType.FourOfAKind
        HandType.TwoPairs to 2 -> HandType.FourOfAKind
        HandType.TwoPairs to 1 -> HandType.FullHouse
        HandType.OnePair to 1 -> HandType.ThreeOfAKind
        HandType.OnePair to 2 -> HandType.ThreeOfAKind
        HandType.HighCard to 1 -> HandType.OnePair
        else -> simpleHandType
    }

    override fun compareTo(other: Hand): Int {
        return when {
            this.handType.value > other.handType.value -> 1
            this.handType.value < other.handType.value -> -1
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

private enum class HandType(val value: Int) {
    HighCard(0),
    OnePair(1),
    TwoPairs(2),
    ThreeOfAKind(3),
    FullHouse(4),
    FourOfAKind(5),
    FiveOfAKind(6),
}