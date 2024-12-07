class Day05 : Day {

    private fun parseInput(input: List<String>): Pair<List<Page>, List<List<Page>>> {
        val (rules, orders) = input.split("")
        val pageNums = rules.map { it.split("|").map { num -> num.toInt() }.toPair() }
        val pages = pageNums.flatMap { it.toList() }.toSet().associateWith { Page(it) }
        pageNums.forEach { (a, b) ->
            pages[a]!!.addPageAfter(pages[b]!!)
            pages[b]!!.addPageBefore(pages[a]!!)
        }

        return pages.values.toList() to orders.map { order -> order.split(",").map { pages[it.toInt()]!! } }
    }

    private fun isValid(order: List<Page>): Boolean {
        return order.zipWithRest().all { (page, rest) -> rest.all { page.isBefore(it) } }
    }

    private fun <T> List<T>.zipWithRest(): List<Pair<T, List<T>>> {
        return this.mapIndexed { index, t -> t to this.subList(index + 1, this.size) }
    }

    override fun part1(input: List<String>): String {
        val (_, orders) = parseInput(input)
        val validOrders = orders.filter { isValid(it) }
        val middlePages = validOrders.map { it[it.size / 2] }
        return middlePages.sumOf { it.pageNum }.toString()
    }

    private tailrec fun sortOrder(order: List<Page>, sorted: List<Page> = emptyList()): List<Page> {
        if(order.isEmpty()) return sorted

        val first = order.find { page -> order.all { other -> page.isBefore(other) } }!!
        val rest = order.filter { it != first }

        return sortOrder(rest, sorted + first)
    }


    override fun part2(input: List<String>): String {
        val (_, orders) = parseInput(input)
        val incorrectOrders = orders.filter { !isValid(it) }
        val sortedOrders = incorrectOrders.map { sortOrder(it) }
        return sortedOrders.map { order -> order[order.size / 2] }.sumOf { it.pageNum }.toString()
    }

}

private data class Page(val pageNum: Int) {
    private val pagesBefore = mutableListOf<Page>()
    private val pagesAfter = mutableListOf<Page>()

    fun addPageBefore(page: Page) {
        pagesBefore.add(page)
    }
    fun addPageAfter(page: Page) {
        pagesAfter.add(page)
    }

    fun isBefore(other: Page): Boolean {
        return !pagesBefore.contains(other)
    }
}