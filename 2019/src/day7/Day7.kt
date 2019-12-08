package day7

import java.io.File

fun main() {
    val opcodes = File("in/day7.txt").readText().trim().split(",").map { it.toInt() }
    val permutations = permute(listOf(5,6,7,8,9))
    var highestThrusterSignal = 0
    for (permutation in permutations) {
        val communicationChannels = permutation.map { mutableListOf(it) }
        communicationChannels[0].add(0)

        val amplifiers = permutation.mapIndexed { i, _ ->
            Amplifier(
                opcodes = opcodes.toMutableList(),
                input = communicationChannels[i],
                output = communicationChannels[(i + 1) % communicationChannels.size],
                id = i
            ) }

        var i = 0
        var lastAmplifierState = Amplifier.RunningState.YIELDED
        while (lastAmplifierState != Amplifier.RunningState.DONE) {
            val curAmplifier = amplifiers[i % amplifiers.size]
            val state = curAmplifier.run()
            if(curAmplifier == amplifiers.last()) lastAmplifierState = state
            i++
        }

        val thrusterSignal = communicationChannels[0][0]
        if (thrusterSignal > highestThrusterSignal) highestThrusterSignal = thrusterSignal
    }
    println("Highest thruster signal: $highestThrusterSignal")
}

class Amplifier(private val opcodes: MutableList<Int>, private val input: MutableList<Int>, private val output: MutableList<Int>, private val id: Int) {
    var ip = 0
    fun run() : RunningState {
        while (opcodes[this.ip] != 99) {
            val opcodeString = opcodes[this.ip].toString().padStart(5, '0')
            val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toInt() - 48] }.reversed()
            when (opcodeString.substring(3).toInt()) {
                1 -> doOp(Integer::sum, parameterModes)
                2 -> doOp(Math::multiplyExact, parameterModes)
                3 -> opcode3()
                4 -> { opcode4(parameterModes); return RunningState.YIELDED }
                5 -> jumpIf({ it != 0 }, parameterModes)
                6 -> jumpIf({ it == 0 }, parameterModes)
                7 -> comparison({a, b -> a < b}, parameterModes)
                8 -> comparison({a, b -> a == b}, parameterModes)
                else -> { ip += 1 }
            }
        }

        return RunningState.DONE
    }


    private fun doOp(op: (Int, Int) -> Int, parameterModes: List<ParameterMode>) {
        val res = getParameterValues(opcodes, ip+1..ip+3, parameterModes.subList(0, 2)).reduce(op)
        val saveIndex = opcodes[ip + 3]
        opcodes[saveIndex] = res
        ip += 4
    }

    private fun opcode3() {
        val inputVal = input.removeAt(0)
        opcodes[opcodes[ip+1]] = inputVal
        ip += 2
    }

    private fun opcode4(parameterModes: List<ParameterMode>) {
        val outputVal = getParameterValue(opcodes, ip + 1, parameterModes[0])
        output.add(outputVal)
        ip += 2
    }

    private fun jumpIf(test: (Int) -> Boolean, parameterModes: List<ParameterMode>) {
        val (num, jumpPos) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
        if(test(num)) ip = jumpPos
        else ip += 3
    }

    private fun comparison(comp: (Int, Int) -> Boolean, parameterModes: List<ParameterMode>) {
        val (a, b) = getParameterValues(opcodes, ip+1..ip+2, parameterModes.subList(0, 2))
        opcodes[opcodes[ip + 3]] = if(comp(a, b)) 1 else 0
        ip += 4
    }

    private fun getParameterValue(opcodes: MutableList<Int>, ip: Int, parameterMode: ParameterMode) =
        if (parameterMode == ParameterMode.POSITION_MODE)
            opcodes[opcodes[ip]]
        else
            opcodes[ip]

    private fun getParameterValues(opcodes: MutableList<Int>, ipRange: IntRange, parameterModes: List<ParameterMode>) =
        ipRange.zip(parameterModes).map { (ip, parameterMode) -> getParameterValue(opcodes, ip, parameterMode) }

    private enum class ParameterMode {
        POSITION_MODE,
        IMMEDIATE_MODE
    }

    enum class RunningState {
        YIELDED,
        DONE
    }
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
