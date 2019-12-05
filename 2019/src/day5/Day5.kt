package day5

fun main() {
    val test = listOf(1,9,10,3,2,3,11,0,99,30,40,50)
    println(run(test))
}

fun run(input: List<Int>) : Int {
    val opcodes = input.toMutableList()
    var ip = 0
    while (opcodes[ip] != 99) {
        when (opcodes[ip]) {
            1 -> ip = opcode1(opcodes, ip)
            2 -> ip = opcode2(opcodes, ip)
        }
    }

    return opcodes[0]
}

fun opcode1(opcodes: MutableList<Int>, ip : Int) : Int {
    val res = opcodes.subList(ip+1, ip+3).map { opcodes[it] }.sum()
    val saveIndex = opcodes[ip+3]
    opcodes[saveIndex] = res
    return ip + 4
}

fun opcode2(opcodes: MutableList<Int>, ip: Int) : Int {
    val res = opcodes.subList(ip+1, ip+3).map { opcodes[it] }.reduce { acc, i -> acc * i }
    val saveIndex = opcodes[ip+3]
    opcodes[saveIndex] = res
    return ip + 4
}
