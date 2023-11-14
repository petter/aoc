import java.math.BigInteger
import kotlin.math.pow

class Day16 : Day {
    private fun parseInput(rawInput: List<String>) : Packet {
        val binaryInput = hexStringToBinaryString(rawInput.first())
        val parser = InputParser(binaryInput)
        return parser.parseOne()
    }

    private fun hexStringToBinaryString(hexString: String): String {
        return hexString.toCharArray().joinToString("") {
            when (it) {
                '0' -> "0000"
                '1' -> "0001"
                '2' -> "0010"
                '3' -> "0011"
                '4' -> "0100"
                '5' -> "0101"
                '6' -> "0110"
                '7' -> "0111"
                '8' -> "1000"
                '9' -> "1001"
                'A' -> "1010"
                'B' -> "1011"
                'C' -> "1100"
                'D' -> "1101"
                'E' -> "1110"
                'F' -> "1111"
                else -> throw Error("Invalid hex char: $it")
            }
        }
    }

    override fun part1(input: List<String>): String {
        val packet = parseInput(input)
        val versionSum = packet.versionSum()
        return versionSum.toString()
    }

    // 8587503954 too low
    private val tests = mapOf(
        "C200B40A82" to 3,
        "04005AC33890" to 54,
        "880086C3E88112" to 7,
        "CE00C43D881120" to 9,
        "D8005AC2A8F0" to 1,
        "F600BC2D8F" to 0,
        "9C005AC2F8F0" to 0,
        "9C0141080250320F1802104A08" to 1,
        "0200749622F" to 10271,
        "02008C09810" to 2,
        "D2FE28" to 2021
    )
    override fun part2(input: List<String>): String {
        tests.forEach { (input, expected) ->
            val packet = parseInput(listOf(input))
            val value = packet.value()
            if (value != expected.toBigInteger()) {
                println("Error in input $input. Expected $expected, got $value")
            }
        }

        val packet = parseInput(input)
        return packet.value().toString()
    }
}


private sealed class Packet(val version: Int, val type: Int) {
    // Sums upp the version number of all packets in this packet, including itself
    abstract fun versionSum(): Int

    abstract fun value(): BigInteger
}

private class LiteralPacket(version: Int, val value: BigInteger) : Packet(version, 4) {
    override fun versionSum(): Int {
        return version
    }

    override fun value(): BigInteger {
        return value
    }
}

private sealed class OperatorPacket(version: Int, type: Int, val subPackets: List<Packet>) : Packet(version, type) {
    companion object {
        fun create(version: Int, type: Int, subPackets: List<Packet>) : OperatorPacket {
            return when(type) {
                0 -> SumOperatorPacket(version, subPackets)
                1 -> ProductOperatorPacket(version, subPackets)
                2 -> MinimumOperatorPacket(version, subPackets)
                3 -> MaximumOperatorPacket(version, subPackets)
                5 -> GreaterThanOperatorPacket(version, subPackets)
                6 -> LessThanOperatorPacket(version, subPackets)
                7 -> EqualOperatorPacket(version, subPackets)
                else -> throw Error("Invalid operator type: $type")
            }
        }
    }

    override fun versionSum(): Int {
        return version + subPackets.sumOf { it.versionSum() }
    }
}

private class SumOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 0, subPackets) {
    override fun value(): BigInteger {
        return subPackets.sumOf { it.value() }
    }
}

private class ProductOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 1, subPackets) {
    override fun value(): BigInteger {
        return subPackets.map { it.value() }.reduce(BigInteger::times)
    }
}

private class MinimumOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 2, subPackets) {
    override fun value(): BigInteger {
        return subPackets.minOf { it.value() }
    }
}

private class MaximumOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 3, subPackets) {
    override fun value(): BigInteger {
        return subPackets.maxOf { it.value() }
    }
}

private class GreaterThanOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 5, subPackets) {
    override fun value(): BigInteger {
        if(subPackets.size != 2) {
            throw Error("GreaterThanOperatorPacket must have exactly 2 sub-packets")
        }

        return if(subPackets[0].value().compareTo(subPackets[1].value()) == 1) BigInteger.ONE else BigInteger.ZERO
    }
}

private class LessThanOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 6, subPackets) {
    override fun value(): BigInteger {
        if(subPackets.size != 2) {
            throw Error("LessThanOperatorPacket must have exactly 2 sub-packets")
        }

        return if(subPackets[0].value().compareTo(subPackets[1].value()) == -1) BigInteger.ONE else BigInteger.ZERO
    }
}

private class EqualOperatorPacket(version: Int, subPackets: List<Packet>) : OperatorPacket(version, 7, subPackets) {
    override fun value(): BigInteger {
        if(subPackets.size != 2) {
            throw Error("EqualOperatorPacket must have exactly 2 sub-packets")
        }

        return if(subPackets[0].value().compareTo(subPackets[1].value()) == 0) BigInteger.ONE else BigInteger.ZERO
    }
}

// Parses a binary string into a list of packets
private class InputParser(input: String) {
    private val reader = StringReader(input)

    private class StringReader(val input: String) {
        private var index = 0

        fun eatChars(amount: Int): String {
            if (index + amount > input.length) {
                throw IndexOutOfBoundsException("Tried to eat $amount chars from index $index, but only ${input.length - index} chars available")
            }

            val res = input.substring(index, index + amount)
            index += amount
            return res
        }

        fun charsLeft() : Int {
            return input.length - index
        }
    }


    fun parse() : List<Packet> {
        val res = mutableListOf<Packet>()

        while(reader.charsLeft() > 10) {
            res.add(parseOne())
        }

        return res
    }

    fun parseOne() : Packet {
        val packetVersion = reader.eatChars(3).binaryToInt()
        val packetType = reader.eatChars(3).binaryToInt()

        // Is literal packet
        if(packetType == 4) {
            return parseLiteralPacket(packetVersion)
        }

        return parseOperatorPacket(packetVersion, packetType)
    }

    private fun parseLiteralPacket(packetVersion: Int) : LiteralPacket {
        var literalValue = ""

        // parse literal value groups
        while(reader.eatChars(1) != "0") {
            literalValue += reader.eatChars(4)
        }

        // parse last group of literal value
        literalValue += reader.eatChars(4)

        return LiteralPacket(packetVersion, literalValue.binaryToBigInteger())
    }

    private fun parseOperatorPacket(packetVersion: Int, packetType: Int) : OperatorPacket {
        val lengthType = reader.eatChars(1).binaryToInt()

        val subPackets = mutableListOf<Packet>()
        if(lengthType == 0) {
            // next 15 bits are a number that represent the total length in bits of the sub-packets
            val subPacketLength = reader.eatChars(15).binaryToInt()
            val subInput = reader.eatChars(subPacketLength)
            val subParser = InputParser(subInput)
            subPackets.addAll(subParser.parse())
        } else {
            // next 11 bits are a number that represent the amount of sub-packets
            val subPacketCount = reader.eatChars(11).binaryToInt()
            for(i in 0 until subPacketCount) {
                val subPacket = parseOne()
                subPackets.add(subPacket)
            }
        }

        return OperatorPacket.create(packetVersion, packetType, subPackets)
    }
}

private fun String.binaryToInt() : Int {
    var res = 0
    for(i in indices) {
        if(this[i] == '1') {
            res += 2.0.pow(this.length - i - 1).toInt()
        }
    }
    return res
}
private fun String.binaryToBigInteger() : BigInteger {
    var res = BigInteger.ZERO
    var mul = BigInteger.ONE
    for(i in indices.reversed()) {
        if(this[i] == '1') {
            res += mul
        }
        mul *= BigInteger.TWO
    }
    return res
}
