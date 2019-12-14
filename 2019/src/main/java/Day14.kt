import java.io.File

typealias Quantity = Long
typealias Recipe = Pair<Quantity, List<Pair<Quantity, Chemical>>>

fun main() {
    val pattern = Regex("(\\d+ [A-Z]+)")
    val input = File("in/day14.txt").readLines()

    val chemicalMap = mutableMapOf<String, Chemical>()
    for (line in input) {
        val reaction = pattern.findAll(line)
            .map { it.value.split(" ") }
            .map { Pair(it[0].toLong(), chemicalMap.getOrPut(it[1], { Chemical(it[1], it[1] == "ORE") })) }
            .toList()
        val chem = reaction.last().second
        chem.recipe = Pair(reaction.last().first, reaction.dropLast(1))
    }

    val fuel = chemicalMap["FUEL"] ?: return
    var totalCoal = 0L
    var fuelProduced = 0L
    // part 1
    totalCoal += fuel.produce(1)
    fuelProduced++
    println("$totalCoal coal required to produce $fuelProduced")

    // part 2
    while (totalCoal < 1000000000000L) {
        totalCoal += fuel.produce(1)
        fuelProduced++
    }
    println("$totalCoal coal required to produce $fuelProduced")

}

class Chemical(val name: String, private val isOre: Boolean = false) {
    var recipe: Recipe? = null
    var excess: Quantity = 0L


    /**
     * Returns ore needed
     */
    fun produce(quantity: Quantity) : Quantity {
        if (isOre) return quantity
        if (recipe == null) error("Missing recipe for $name")

        var coalProduced = 0L
        while (excess < quantity) {
            coalProduced += recipe!!.second.map { (amountNeeded, chemical) -> chemical.produce(amountNeeded) }.sum()
            excess += recipe!!.first
        }

        excess -= quantity
        return coalProduced
    }

}