import Opcode.*

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

class Intcode(input: List<String>) {
    private var ip = 0L
    private val memory: MutableList<Long>
    private var running = false

    init {
        this.memory = parseIntcodeProgram(input)
    }

    fun runProgram(): Long {
        running = true
        while(running) {
            parseInstruction()
        }

        return read(0L)
    }

    private fun parseInstruction() {
        val instruction = read()

        when (Opcode.from(instruction)) {
            ADD -> doAdd()
            MUL -> doMul()
            HALT -> halt()
        }
    }

    private fun doAdd() {
        val num1Address = read(ip + 1L)
        val num2Address = read(ip + 2L)
        val resultAddress = read(ip + 3L)

        val num1 = read(num1Address)
        val num2 = read(num2Address)
        write(resultAddress, num1 + num2)

        ip += 4
    }

    private fun doMul() {
        val num1Address = read(ip + 1L)
        val num2Address = read(ip + 2L)
        val resultAddress = read(ip + 3L)

        val num1 = read(num1Address)
        val num2 = read(num2Address)
        write(resultAddress, num1 * num2)

        ip += 4
    }

    private fun halt() {
        running = false
    }

    fun read(): Long {
        return read(ip)
    }
    fun read(address: Long): Long {
        return memory[address.toInt()]
    }

    fun write(value: Long) {
        write(ip, value)
    }
    fun write(address: Long, value: Long) {
        memory[address.toInt()] = value
    }

}
