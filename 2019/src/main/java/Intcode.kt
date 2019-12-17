import java.io.File

class IntCodeProgram(private val opcodes: InfiniteArrayList, val inputChannel: MutableList<Long> = mutableListOf(), val outputChannel: MutableList<Long> = mutableListOf()) {
    private var ip = 0
    private var relativeBase = 0

    companion object {
        fun fromFile(filePath: String): IntCodeProgram {
            val opcodes = File("in/day15.txt").readText().trim().split(",").map { it.toLong() }.toInfiniteArrayList()
            return IntCodeProgram(opcodes)
        }
    }

    fun run() : RunState {
        while (opcodes[ip] != 99L) {
            val opcodeString = opcodes[ip].toString().padStart(5, '0')
            val parameterModes = opcodeString.substring(0, 3).map { ParameterMode.values()[it.toString().toInt()] }.reversed()
            when (opcodeString.substring(3).toInt()) {
                1 -> doOp(Long::plus, parameterModes)
                2 -> doOp(Long::times, parameterModes)
                3 -> opcode3(parameterModes)
                4 -> { opcode4(parameterModes); return RunState.YIELDED }
                5 -> jumpIf({ it != 0L }, parameterModes)
                6 -> jumpIf({ it == 0L }, parameterModes)
                7 -> comparison({a, b -> a < b}, parameterModes)
                8 -> comparison({a, b -> a == b}, parameterModes)
                9 -> opcode9(parameterModes)
                else -> error("Unsupported opcode")
            }
        }

        return RunState.DONE
    }

    private fun doOp(op: (Long, Long) -> Long, parameterModes: List<ParameterMode>) {
        val res = getParameterValues(ip+1..ip+3, parameterModes.subList(0, 2)).reduce(op)
        write(res, ip + 3, parameterModes.last())
        ip += 4
    }

    private fun opcode3(parameterModes: List<ParameterMode>) {
        val input = inputChannel.removeAt(0)
        write(input, ip + 1, parameterModes[0])
        ip += 2
    }

    private fun opcode4(parameterModes: List<ParameterMode>) {
        val outputVal = getParameterValue(ip + 1, parameterModes[0])
        outputChannel.add(outputVal)
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

    enum class RunState {
        YIELDED,
        DONE
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

fun List<Long>.toInfiniteArrayList() : InfiniteArrayList {
    return InfiniteArrayList(this)
}
