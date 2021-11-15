import Opcode.*
import ParameterMode.*

enum class Opcode(val int: Long) {
    ADD(1L),
    MUL(2L),
    HALT(99L);

    companion object {
        fun from(num: Long): Opcode {
            return when (num) {
                1L -> ADD
                2L -> MUL
                99L -> HALT
                else -> throw IllegalArgumentException("Unknown opcode: $num")
            }
        }
    }
}

enum class ParameterMode(val int: Long) {
    POSITION(0L),
    IMMEDIATE(1L);

    companion object {
        fun from(num: Long): ParameterMode {
            return when (num) {
                0L -> POSITION
                1L -> IMMEDIATE
                else -> throw IllegalArgumentException("Unknown parameter mode: $num")
            }
        }
    }
}

class Intcode(input: List<String>) {
    private var ip = 0L
    private val memory: MutableList<Long>
    private var running = false

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

    fun write(value: Long, parameterMode: ParameterMode = POSITION) {
        write(ip, value, parameterMode)
    }
    fun write(address: Long, value: Long, parameterMode: ParameterMode = POSITION) {
        val writeAddress = if(parameterMode === IMMEDIATE) address else read(address, IMMEDIATE)
        memory[writeAddress.toInt()] = value
    }

    private fun parseInstruction(num: Long): Pair<Triple<ParameterMode, ParameterMode, ParameterMode>, Opcode> {
        val paramModes = Triple(
            ParameterMode.from(getDigitAtPosition(num, 4)),
            ParameterMode.from(getDigitAtPosition(num, 3)),
            ParameterMode.from(getDigitAtPosition(num, 2)),
        )

        val opcode = Opcode.from(num % 100)

        return paramModes to opcode
    }

}
