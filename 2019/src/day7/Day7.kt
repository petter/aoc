package day7

import java.io.File

var inputSignal = 0
var i = 0

fun main() {
    val opcodes = File("in/day7.txt").readText().trim().split(",").map { it.toInt() }
    val permutations = permute(listOf(0,1,2,3,4))
    var highestThrusterSignal = 0
    for (permutation in permutations) {
        inputSignal = 0
        for (phaseSetting in permutation) {
            run(opcodes, phaseSetting, inputSignal)
        }
        println("$permutation: $inputSignal")
        if (inputSignal > highestThrusterSignal) highestThrusterSignal = inputSignal
    }
    println("Highest thruster signal: $highestThrusterSignal")
}

/* Heap's algorithm
*  From Rosetta Code(https://rosettacode.org/wiki/Permutations#Kotlin)
* */
fun <T> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun run(input: List<Int>, phaseSetting: Int, inputSignal: Int) : Int {
    val opcodes = input.toMutableList()
    var ip = 0
    while (opcodes[ip] != 99) {
        val opcodeString = opcodes[ip].toString().padStart(5, '0')
        val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toInt() - 48] }.reversed()
        val opcode = opcodeString.substring(3).toInt()

        val opcode3 = opcode3Gen(mutableListOf(phaseSetting, inputSignal))
        val func = when (opcode) {
            1 -> ::opcode1
            2 -> ::opcode2
            3 -> opcode3
            4 -> ::opcode4
            5 -> ::opcode5
            6 -> ::opcode6
            7 -> ::opcode7
            8 -> ::opcode8
            else -> ::opcode0
        }

        ip = func(opcodes, ip, parameterModes)
    }

    return opcodes[0]
}

fun opcode0(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    // NO OP
    return ip
}

fun opcode1(opcodes: MutableList<Int>, ip : Int, parameterModes: List<ParameterMode>) : Int {
    val res = getParameterValues(opcodes, ip+1..ip+3, parameterModes.subList(0, 2)).sum()
    val saveIndex = opcodes[ip + 3]
    opcodes[saveIndex] = res
    return ip + 4
}

fun opcode2(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val res = getParameterValues(opcodes, ip+1..ip+3, parameterModes.subList(0, 2)).reduce { acc, i -> acc * i }
    val saveIndex = opcodes[ip+3]
    opcodes[saveIndex] = res
    return ip + 4
}

fun opcode3Gen(inputs: List<Int>) = fun (opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    println("Input: ${inputs[i % 2]}")
    opcodes[opcodes[ip+1]] = inputs[(i++) % 2]
    return ip + 2
}

fun opcode4(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    inputSignal = getParameterValue(opcodes, ip + 1, parameterModes[0])
    println("Output: $inputSignal")
    return ip + 2
}

fun opcode5(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val (test, jumpPos) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
    if(test != 0) return jumpPos
    return ip + 3
}

fun opcode6(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val (test, jumpPos) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
    if(test == 0) return jumpPos
    return ip + 3
}

fun opcode7(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val (a, b) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
    opcodes[opcodes[ip + 3]] = if(a < b) 1 else 0
    return ip + 4
}

fun opcode8(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val (a, b) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
    opcodes[opcodes[ip + 3]] = if(a == b) 1 else 0
    return ip + 4
}

fun getParameterValue(opcodes: MutableList<Int>, ip: Int, parameterMode: ParameterMode) =
    if (parameterMode == ParameterMode.POSITION_MODE)
        opcodes[opcodes[ip]]
    else
        opcodes[ip]

fun getParameterValues(opcodes: MutableList<Int>, ipRange: IntRange, parameterModes: List<ParameterMode>) : List<Int> {
    return ipRange.zip(parameterModes).map { (ip, parameterMode) -> getParameterValue(opcodes, ip, parameterMode) }
}

enum class ParameterMode {
    POSITION_MODE,
    IMMEDIATE_MODE
}

