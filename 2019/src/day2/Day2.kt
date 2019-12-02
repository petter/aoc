package day2

import java.io.File

fun main() {

    val opcodes = File("in/day2.txt").readText().trim().split(",").map { it.toInt() }.toMutableList()
    opcodes[1] = 12
    opcodes[2] = 2

    for (i in 0..opcodes.size step 4) {
        println("\nindex $i")
        val opcode = opcodes[i]
        println("opcode $opcode")
        if(opcode == 99) break

        val vals = opcodes.subList(i+1, i+3).map { opcodes[it] }
        val saveIndex = opcodes[i+3]
        val res = if (opcode == 1) vals.sum() else vals.reduce {acc, i -> acc * i}
        println(vals)
        println(res)

        opcodes[saveIndex] = res
    }
    println(opcodes)
}