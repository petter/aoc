package day13

import java.awt.*
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.io.File
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.Timer

fun main() {
    EventQueue.invokeLater {
        val frame = Canvas()
        frame.isVisible = true
    }
}

class Canvas : JFrame() {
    init {
        initUI()
    }
    private fun initUI() {

        val opcodes = File("in/day13.txt").readText().trim().split(",").map { it.toLong() }.toInfiniteArrayList()
        val program = IntCodeProgram(opcodes = opcodes)
        val game = Game(program)
        add(Board(game))

        title = "Day 13"

        isResizable = false
        pack()

        setLocationRelativeTo(null)
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    }
}

class Board(val game: Game) : JPanel(), ActionListener {

    private var timer: Timer? = null
    private val blockSize = 10
    private val aiTimerDelay = 5
    private val playerTimerDelay = 150

    private val blockColors = listOf(
        Color(0,0,0),
        Color(255,255,255),
        Color(200,200,200),
        Color(0,255,0),
        Color(255,0,0)
    )
    private var leftDirection = false
    private var rightDirection = true

    init {

        addKeyListener(TAdapter())
        background = Color.black
        isFocusable = true
        preferredSize = Dimension(500, 500)
        initGame()
    }

    private fun initGame() {

        while (!game.map.containsValue(3)) game.run()
        timer = Timer(1, this)
        timer!!.start()
    }

    private fun drawGame(g: Graphics) {
        game.map.forEach { (x,y), block ->
            g.color = blockColors[block]
            g.fillRect(x*blockSize,y*blockSize,blockSize,blockSize)
        }


    }

    public override fun paintComponent(g: Graphics) {
        super.paintComponent(g)

        drawGame(g)
        doDrawing(g)
    }

    private fun doDrawing(g: Graphics) {
        Toolkit.getDefaultToolkit().sync()
    }

    private inner class TAdapter : KeyAdapter() {

        override fun keyPressed(e: KeyEvent?) {

            val key = e!!.keyCode

            when (key) {
                KeyEvent.VK_A -> game.aiActive = !game.aiActive
                KeyEvent.VK_LEFT -> game.program.input = -1
                KeyEvent.VK_RIGHT -> game.program.input = 1
            }

        }

        override fun keyReleased(e: KeyEvent?) {
            val key = e!!.keyCode
            if (key == KeyEvent.VK_LEFT || key == KeyEvent.VK_RIGHT) game.program.input = 0
        }
    }

    override fun actionPerformed(p0: ActionEvent?) {
        timer!!.delay = if(game.aiActive) aiTimerDelay else playerTimerDelay
        gameLoop()
    }

    private fun gameLoop() {

        game.run()
        println("Score: ${game.score}")
        repaint()
    }
}

class Game(val program: IntCodeProgram) {
    val map = mutableMapOf<Pair<Int, Int>, Int>()
    var score = 0
    var aiActive = true
    var ballX = 0
    var paddleX = 0
    fun run() {
        var intcodeState = IntCodeProgram.RunState.YIELDED
        var i = 0
        while (intcodeState != IntCodeProgram.RunState.DONE && (i % 3 != 0 || i == 0 || program.outputChannel[i-1] == 0L)) {
            intcodeState = program.run()
            i++
        }

        while(program.outputChannel.isNotEmpty()) {
            val x = program.outputChannel.removeAt(0).toInt()
            val y = program.outputChannel.removeAt(0).toInt()
            val blockId = program.outputChannel.removeAt(0).toInt()
            if (x != -1) map[Pair(x, y)] = blockId
            else score = blockId

            if (blockId == 4) ballX = x
            if (blockId == 3) paddleX = x
        }

        if (!aiActive) return

        if (ballX < paddleX) program.input = -1
        else if (ballX > paddleX) program.input = 1
        else program.input = 0
    }

}

class IntCodeProgram(private val opcodes: InfiniteArrayList, val inputChannel: MutableList<Long> = mutableListOf(), val outputChannel: MutableList<Long> = mutableListOf()) {
    private var ip = 0
    private var relativeBase = 0

    var input = 0
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
        write(input.toLong(), ip + 1, parameterModes[0])
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
