import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

fun main() {
    val input = File("in/day8.txt").readText().trim().toCharArray().map { it.toString().toInt() }
    val width = 25
    val height = 6
    val layerInputs = input.withIndex().groupBy { it.index / (width * height) }.map { group -> group.value.map { it.value } }
    val fewestZeroLayer = layerInputs.sortedBy { layer -> layer.count { it == 0 } }[0]
    println(fewestZeroLayer.count {it == 1} * fewestZeroLayer.count {it == 2})

    val combinedLayers = layerInputs.reduce {acc, cur -> acc.zip(cur).map { (a, b) -> if(a == 2) b else a }}
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
    val colors = arrayOf(Color(0,0,0), Color(255,255,255), Color(255, 255, 255, 255))
    combinedLayers.forEachIndexed { index, color -> image.setRGB(index % width, index / width, colors[color].rgb) }
    ImageIO.write(image, "png", File("out/day8.png"))
}