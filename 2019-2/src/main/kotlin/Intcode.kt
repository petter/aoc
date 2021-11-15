import Opcode.*
import ParameterMode.*

enum class Opcode(val num: Long) {
    ADD(1L),
    MUL(2L),
    INPUT(3L),
    OUTPUT(4L),
    JNZ(5L),
    JZ(6L),
    LT(7L),
    EQ(8L),
    HALT(99L);

    companion object {
        fun from(num: Long): Opcode {
            return values().find { it.num == num } ?: throw IllegalArgumentException("Unknown opcode: $num")
        }
    }
}

enum class ParameterMode(val num: Long) {
    POSITION(0L),
    IMMEDIATE(1L);

    companion object {
        fun from(num: Long): ParameterMode {
            return values().find { it.num == num } ?: throw IllegalArgumentException("Unknown parameter mode: $num")
        }
    }
}

class Intcode(input: List<String>, private val inputBuffer: MutableList<String> = mutableListOf()) {
    private var ip = 0L
    val memory: MutableList<Long>
    private var running = false
    val outputBuffer = mutableListOf<Long>()

    init {
        memory = parseIntcodeProgram(input)
    }

    fun runProgram(): Long {
        running = true
        while(running) {
            performInstruction()
        }

        return read(0L, IMMEDIATE)
    }

    private fun performInstruction() {
        val instructionNum = read(IMMEDIATE)
        val (paramModes, opcode) = parseInstruction(instructionNum)

        when (opcode) {
            ADD -> doAdd(paramModes)
            MUL -> doMul(paramModes)
            INPUT -> doInput(paramModes)
            OUTPUT -> doOutput(paramModes)
            JNZ -> doJumpNotZero(paramModes)
            JZ -> doJumpZero(paramModes)
            LT -> doLessThan(paramModes)
            EQ -> doEquals(paramModes)
            HALT -> halt()
        }
    }

    private fun doAdd(paramModes : Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val num1 = read(ip + 1L, paramModes.first)
        val num2 = read(ip + 2L, paramModes.second)
        write(ip + 3L, num1 + num2, paramModes.third)
        ip += 4
    }

    private fun doMul(paramModes : Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val num1 = read(ip + 1L, paramModes.first)
        val num2 = read(ip + 2L, paramModes.second)
        write(ip + 3L, num1 * num2, paramModes.third)
        ip += 4
    }

    private fun doInput(paramModes : Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val input = inputBuffer.removeFirstOrNull() ?: let {
            print("Input: ")
            readLine()!!.trim()
        }
        write(ip + 1L, input.toLong(), paramModes.first)
        ip += 2
    }

    private fun doOutput(paramModes : Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val output = read(ip + 1L, paramModes.first)
        outputBuffer.add(output)
        ip += 2
    }

    private fun doJumpNotZero(paramModes : Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val param = read(ip + 1L, paramModes.first)
        val jumpAddr = read(ip + 2L, paramModes.second)
        if (param != 0L) {
            ip = jumpAddr
        } else {
            ip += 3
        }
    }

    private fun doJumpZero(paramModes: Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val param = read(ip + 1L, paramModes.first)
        val jumpAddr = read(ip + 2L, paramModes.second)
        if (param == 0L) {
            ip = jumpAddr
        } else {
            ip += 3
        }
    }

    private fun doLessThan(paramModes: Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val num1 = read(ip + 1L, paramModes.first)
        val num2 = read(ip + 2L, paramModes.second)
        val writeValue = if (num1 < num2) 1L else 0L
        write(ip + 3L, writeValue, paramModes.third)
        ip += 4
    }

    private fun doEquals(paramModes: Triple<ParameterMode, ParameterMode, ParameterMode>) {
        val num1 = read(ip + 1L, paramModes.first)
        val num2 = read(ip + 2L, paramModes.second)
        val writeValue = if (num1 == num2) 1L else 0L
        write(ip + 3L, writeValue, paramModes.third)
        ip += 4
    }


    private fun halt() {
        running = false
    }

    fun read(parameterMode: ParameterMode = POSITION): Long {
        return read(ip, parameterMode)
    }
    fun read(address: Long, parameterMode: ParameterMode = POSITION): Long {
        val readAddress = if(parameterMode == IMMEDIATE) address else read(address, IMMEDIATE)
        return memory[readAddress.toInt()]
    }

    fun write(address: Long, value: Long, parameterMode: ParameterMode = POSITION) {
        val writeAddress = if(parameterMode === IMMEDIATE) address else read(address, IMMEDIATE)
        memory[writeAddress.toInt()] = value
    }

    private fun parseInstruction(num: Long): Pair<Triple<ParameterMode, ParameterMode, ParameterMode>, Opcode> {
        val paramModes = Triple(
            ParameterMode.from(getDigitAtPosition(num, 2)),
            ParameterMode.from(getDigitAtPosition(num, 3)),
            ParameterMode.from(getDigitAtPosition(num, 4)),
        )

        val opcode = Opcode.from(num % 100)

        return paramModes to opcode
    }

    private fun getDigitAtPosition(num : Long, position : Int) : Long =
        num.toString().padStart(5, '0')[4 - position].toString().toLong()

}
