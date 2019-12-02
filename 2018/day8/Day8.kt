package day8

import java.io.File

var i = 0

fun main() {
    val numbers = File("2018/day8/input.txt").readText().trim().split(" ").map {it.toInt()}
    val root = createTree(numbers, 0)
    println("1) Sum of metadata: ${root.sum()}")
    println("2) Sum of metadata: ${root.metadataSum()}")
}

fun createTree(numbers: List<Int>, depth: Int) : Node {
    val numChildren = numbers[i++]
    val numMetadata = numbers[i++]
    val r = (0..numChildren).drop(1)

    val children = r.map { createTree(numbers, depth + 1) }
    val metadata = numbers.subList(i, i + numMetadata)
    i += numMetadata

    return Node(children, metadata)
}

class Node(private val children: List<Node>, private val metadata: List<Int>) {
    fun sum() : Int = metadata.sum() + children.fold(0, {acc, node -> acc + node.sum()})

    fun metadataSum() : Int =
        if(children.isEmpty()) metadata.sum()
        else metadata.mapNotNull { children.getOrNull(it - 1) }
                .map {it.metadataSum()}
                .sum()


}

