package day5

import java.io.File

fun main() {
    val opcodes = File("in/day5.txt").readText().trim().split(",").map { it.toInt() }
    run(opcodes)
}

fun run(input: List<Int>) : Int {
    val opcodes = input.toMutableList()
    var ip = 0
    while (opcodes[ip] != 99) {
        val opcodeString = opcodes[ip].toString().padStart(5, '0')
        val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toInt() - 48] }.reversed()
        val opcode = opcodeString.substring(3).toInt()

        val func = when (opcode) {
            1 -> ::opcode1
            2 -> ::opcode2
            3 -> ::opcode3
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

fun opcode3(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    print("Input: ")
    var input = readLine()
    if(input == null) input = "0"
    opcodes[opcodes[ip+1]] = input.toInt()
    return ip + 2
}

fun opcode4(opcodes: MutableList<Int>, ip: Int, parameterModes: List<ParameterMode>) : Int {
    val output = getParameterValue(opcodes, ip + 1, parameterModes[0])
    println("Diagnostic output: $output")
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