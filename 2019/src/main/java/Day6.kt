import java.io.File

val spaceObjectsMap = HashMap<String, SpaceObject>()

fun main() {
    val input = File("in/day6.txt").readLines()
    input.forEach(::parseOrbitString)
    println("Orbit check sum: ${orbitCountChecksum()}")
    val you = spaceObjectsMap["YOU"]
    val san = spaceObjectsMap["SAN"]
    if(you == null || san == null) {
        error("YOU or SAN is missing")
    }
    println("Steps to Santa: ${bfsShortestPath(you, san)}")

}

fun parseOrbitString(s: String) {
    val spaceObjects = s.split(")").map { name -> spaceObjectsMap.getOrPut(name, { SpaceObject(name) } ) }
    spaceObjects[1].orbits = spaceObjects[0]
    spaceObjects[0].orbitsMe.add(spaceObjects[1])
}

fun orbitCountChecksum() : Int =
    spaceObjectsMap.values.map { it.countOrbits() }.sum()

fun bfsShortestPath(from: SpaceObject, to: SpaceObject) : Int {
    from.visited = true
    var curLayer = mutableListOf(from)
    var i = 0
    while (curLayer.isNotEmpty()) {
        var nextLayer = mutableListOf<SpaceObject>()
        for (node in curLayer) {
            for (neighbor in node.neighbors()) {
                if (neighbor == to) return i - 1
                if (!neighbor.visited) {
                    neighbor.visited = true
                    nextLayer.add(neighbor)
                }
            }
        }
        curLayer = nextLayer
        i++
    }

    return -1
}

class SpaceObject(val name : String) {
    var orbits : SpaceObject? = null
    var orbitsMe : MutableList<SpaceObject> = mutableListOf()
    var visited = false

    fun countOrbits() : Int {
        return if(orbits != null)  orbits!!.countOrbits() + 1 else 0
    }

    fun neighbors() : List<SpaceObject> {
        val res = orbitsMe.toMutableList()
        if (orbits != null) res.add(orbits!!)
        return res.toList()
    }

}
