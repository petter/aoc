package day6

import java.io.File

val spaceObjectsMap = HashMap<String, SpaceObject>()

fun main() {
    val input = File("in/day6.txt").readLines()
    input.forEach(::parseOrbitString)
    println(orbitCountChecksum())
}

fun parseOrbitString(s: String) {
    val spaceObjects = s.split(")").map { name -> spaceObjectsMap.getOrPut(name, { SpaceObject(name) } ) }
    spaceObjects[1].orbits = spaceObjects[0]
}

fun orbitCountChecksum() : Int =
    spaceObjectsMap.values.map { it.countOrbits() }.sum()



class SpaceObject(val name : String) {
    var orbits : SpaceObject? = null
    
    fun countOrbits() : Int {
        return if(orbits != null)  orbits!!.countOrbits() + 1 else 0
    }
}
