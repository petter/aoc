package day2

import java.io.File

fun main() {

    val opcodes = File("in/day2.txt").readText().trim().split(",").map { it.toInt() }
    for (noun in 0..99) {
        for (verb in 0..99) {
            val res = run(opcodes, noun, verb)
            if(res == 19690720) {
                println("Noun: $noun, Verb: $verb. 100 * noun + verb = ${100 * noun + verb}")
                return;
            }
        }
    }
    println(opcodes)
}



fun run(input: List<Int>, noun : Int, verb : Int) : Int {

    val opcodes = input.toMutableList()
    opcodes[1] = noun
    opcodes[2] = verb
    for (i in 0..opcodes.size step 4) {
        val opcode = opcodes[i]
        if(opcode == 99) break

        val vals = opcodes.subList(i+1, i+3).map { opcodes[it] }
        val saveIndex = opcodes[i+3]
        val res = if (opcode == 1) vals.sum() else vals.reduce {acc, i -> acc * i}

        opcodes[saveIndex] = res
    }

    return opcodes[0]
}