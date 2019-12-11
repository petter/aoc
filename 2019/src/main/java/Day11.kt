package day11

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import kotlin.collections.ArrayList

fun main() {
    val opcodes = File("in/day11.txt").readText().trim().split(",").map { it.toLong() }
    val program = IntCodeProgram(opcodes.toInfiniteArrayList(), mutableListOf(), mutableListOf())
    val robot = PaintingRobot(program)
    robot.paint()
    println("Number of painted tiles: ${robot.paintedTiles.values.size}")
    println(robot.paintedTiles.values)

    val maxX = robot.paintedTiles.map { (vec, _) -> vec.x }.max()!!
    val minX = robot.paintedTiles.map { (vec, _) -> vec.x }.min()!!
    val maxY = robot.paintedTiles.map { (vec, _) -> vec.y }.max()!!
    val minY = robot.paintedTiles.map { (vec, _) -> vec.y }.min()!!
    println("x ($minX, $maxX) y ($minY, $maxY)")
    val image = BufferedImage(maxX - minX + 1, maxY - minY + 1, BufferedImage.TYPE_3BYTE_BGR)
    robot.paintedTiles.forEach {(vec, color) -> image.setRGB(vec.x - minX, vec.y - minY, if(color == 1) Color(255, 255, 255).rgb else Color(0,0,0).rgb) }
    ImageIO.write(image, "png", File("out/day11.png"))

}

class PaintingRobot(val program: IntCodeProgram) {

    var facing = Facing.Up
    private val facings = Facing.values()
    var position = Vector2(0, 0)
    val paintedTiles = mutableMapOf(Pair(position, 1))

    fun paint() {
        while (true) {
            val tileColor = paintedTiles.getOrDefault(position, 0)
            program.inputChannel.add(tileColor.toLong())
            val state = program.run()
            if(state == IntCodeProgram.RunState.DONE) return
            program.run()

            val color = program.outputChannel.removeAt(0).toInt()
            paintedTiles[position] = color

            turn(program.outputChannel.removeAt(0).toInt())
            step()
        }
    }

    fun turn(input: Int) {
        val turn = if(input == 0) 3 else 1
        facing = facings[(facings.indexOf(facing) + turn) % facings.size]
    }

    fun step() {
        position += facing.vector2
    }

    enum class Facing(val vector2: Vector2) {
        Left(Vector2(-1, 0)),
        Up(Vector2(0, -1)),
        Right(Vector2(1, 0)),
        Down(Vector2(0, 1)),
    }
}

class IntCodeProgram(private val opcodes: InfiniteArrayList, val inputChannel: MutableList<Long>, val outputChannel: MutableList<Long>) {
    private var ip = 0
    private var relativeBase = 0

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
        var inputVal = 0L
        if(inputChannel.isEmpty()) {
            print("Input: ")
            inputVal = readLine()?.toLong() ?: 0L
        } else {
            inputVal = inputChannel.removeAt(0)
        }
        write(inputVal, ip + 1, parameterModes[0])
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

data class Vector2(val x: Int, val y: Int) {
    operator fun plus(other: Vector2) = Vector2(x + other.x, y + other.y)
    operator fun minus(other: Vector2) = Vector2(x - other.x, y - other.y)
    operator fun times(number: Int) = Vector2(x * number, y * number)
}

fun List<Long>.toInfiniteArrayList() : InfiniteArrayList {
    return InfiniteArrayList(this)
}
