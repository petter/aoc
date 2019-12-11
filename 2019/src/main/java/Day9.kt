package day9

import java.io.File

fun main() {
    val opcodes = File("in/day9.txt").readText().trim().split(",").map { it.toLong() }
    //val opcodes = listOf(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map { it.toLong() }
    //val opcodes = listOf(1102,34915192,34915192,7,4,7,99,0).map { it.toLong() }
    //val opcodes = listOf(104L,1125899906842624L,99L)
    //val opcodes = listOf(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    //    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    //    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99).map {it.toLong()}
    //val opcodes = listOf(1, 0, 0, 0, 4, 0, 99).map { it.toLong() }

    IntCodeProgram(opcodes.toInfiniteArrayList()).run()
}

fun List<Long>.toInfiniteArrayList() : InfiniteArrayList {
    return InfiniteArrayList(this)
}

class IntCodeProgram(private val opcodes: MutableList<Long>) {
    private var ip = 0
    private var relativeBase = 0

    fun run() {
        while (opcodes[ip] != 99L) {
            val opcodeString = opcodes[ip].toString().padStart(5, '0')
            val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toString().toInt()] }.reversed()
            when (opcodeString.substring(3).toInt()) {
                1 -> doOp(Long::plus, parameterModes)
                2 -> doOp(Long::times, parameterModes)
                3 -> opcode3(parameterModes)
                4 -> opcode4(parameterModes)
                5 -> jumpIf({ it != 0L }, parameterModes)
                6 -> jumpIf({ it == 0L }, parameterModes)
                7 -> comparison({a, b -> a < b}, parameterModes)
                8 -> comparison({a, b -> a == b}, parameterModes)
                9 -> opcode9(parameterModes)
                else -> error("Unsupported opcode")
            }
        }
    }

    private fun doOp(op: (Long, Long) -> Long, parameterModes: List<ParameterMode>) {
        val res = getParameterValues(ip+1..ip+3, parameterModes.subList(0, 2)).reduce(op)
        write(res, ip + 3, parameterModes.last())
        ip += 4
    }

    private fun opcode3(parameterModes: List<ParameterMode>) {
        print("Input: ")
        val inputVal = readLine()?.toLong() ?: 0L
        write(inputVal, ip + 1, parameterModes[0])
        ip += 2
    }

    private fun opcode4(parameterModes: List<ParameterMode>) {
        val outputVal = getParameterValue(ip + 1, parameterModes[0])
        println("Output: $outputVal")
        ip += 2
    }

    private fun jumpIf(test: (Long) -> Boolean, parameterModes: List<ParameterMode>) {
        val (num, jumpPos) = getParameterValues(ip+1..ip+2, parameterModes.subList(0, 2))
        if(test(num)) ip = jumpPos.toInt()
        else ip += 3
    }

    private fun comparison(comp: (Long, Long) -> Boolean, parameterModes: List<ParameterMode>) {
        val (a, b) = getParameterValues(ip+1..ip+2, parameterModes.subList(0, 2))
        val res = if(comp(a, b)) 1L else 0L
        write(res, ip + 3, parameterModes.last())
        ip += 4
    }

    private fun opcode9(parameterModes: List<ParameterMode>) {
        relativeBase += getParameterValue(ip + 1, parameterModes[0]).toInt()
        ip += 2
    }

    private fun getParameterValue(ip: Int, parameterMode: ParameterMode) =
        when (parameterMode) {
            ParameterMode.POSITION_MODE  -> opcodes[opcodes[ip].toInt()]
            ParameterMode.IMMEDIATE_MODE -> opcodes[ip]
            ParameterMode.RELATIVE_MODE  -> opcodes[opcodes[ip].toInt() + relativeBase]
        }

    private fun getParameterValues(ipRange: IntRange, parameterModes: List<ParameterMode>) =
        ipRange.zip(parameterModes).map { (ip, parameterMode) -> getParameterValue(ip, parameterMode) }

    private fun write(value: Long, ip: Int, parameterMode: ParameterMode) = when (parameterMode) {
        ParameterMode.POSITION_MODE  -> opcodes[opcodes[ip].toInt()] = value
        ParameterMode.RELATIVE_MODE  -> opcodes[opcodes[ip].toInt() + relativeBase] = value
        ParameterMode.IMMEDIATE_MODE -> error("Unsupported parameter mode")
    }

    private enum class ParameterMode {
        POSITION_MODE,
        IMMEDIATE_MODE,
        RELATIVE_MODE
    }
}

class InfiniteArrayList(): ArrayList<Long>() {
    constructor(initialList: List<Long>) : this() {
        addAll(initialList)
    }

    private fun extend(index: Int) {
        for (i in 0..(index-size)) {
            super.add(0L)
        }
    }

    override fun get(index: Int): Long {
        if(index >= size) extend(index)
        return super.get(index)
    }

    override fun set(index: Int, element: Long): Long {
        if(index >= size) extend(index)
        return super.set(index, element)
    }

}
