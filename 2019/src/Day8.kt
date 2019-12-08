import java.io.File

fun main() {
    val input = File("in/day8.txt").readText().trim().toCharArray().map { it.toString().toInt() }
    val width = 25
    val height = 6
    val layerInputs = input.withIndex().groupBy { it.index / (width * height) }.map { group -> group.value.map { it.value } }
    val fewestZeroLayer = layerInputs.sortedBy { layer -> layer.count { it == 0 } }[0]
    println(fewestZeroLayer.count {it == 1} * fewestZeroLayer.count {it == 2})

}