package day9

import java.math.BigInteger

const val numPlayers = 400
var curMarble = Marble()
val players = MutableList<Long>(numPlayers) { 0 }
val startNode = curMarble

fun main() {
    var curPlayer = 0
    var lastMarbleWorth = 0
    var round = 1
    while (round < 71864 * 100 + 1) {
        insertMarble(curPlayer)
        curPlayer = (curPlayer + 1) % players.size
        round++
    }

    println(lastMarbleWorth)

    println("Highscore: ${players.max()}")
    println(curMarble)
}

fun printSequence() {
    print("${startNode.num} ")
    var cur = curMarble.right
    while(cur != startNode) {
        print("${cur.num} ")
    }
        println()
}

fun insertMarble(curPlayer : Int) : Int {
    var marble = Marble()

    if(marble.num % 23 == 0) {
        var score = marble.num
        marble = curMarble
        for (i in 0..6) {
            marble = marble.left
        }

        score += marble.removeMarble().num
        curMarble = marble.right

        players[curPlayer] = players[curPlayer].plus(score)
        return score
    }

    curMarble = curMarble.right.insertRight(marble)
    return 0
}

class Marble {
    var num : Int = 0
    var left : Marble = this
    var right : Marble = this

    companion object {
        var curId = 0
        fun getId() = curId++
    }

    constructor() {
        num = getId()
    }

    fun insertRight(marble : Marble) : Marble {
        marble.left = this
        marble.right = right
        right.left = marble
        right = marble
        return marble
    }

    fun removeMarble() : Marble {
        left.right = right
        right.left = left
        return this
    }

    override fun toString(): String = "${left.num} <-- $num --> ${right.num}"
}