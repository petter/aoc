package day9

const val numPlayers = 10
var curMarble = Marble()
val players = MutableList(numPlayers) { 0 }
const val highestMarblePoint = 1618
val keptMarbles = MutableList<Marble?>(numPlayers) { null }

fun main() {
    var curPlayer = 0
    var lastMarbleWorth = 0
    while (lastMarbleWorth < highestMarblePoint) {
        lastMarbleWorth = insertMarble(curPlayer)
        curPlayer = (curPlayer + 1) % players.size
    }

    println(lastMarbleWorth)

    println("Highscore: ${players.max()}")
    println(curMarble)
}

fun insertMarble(curPlayer : Int) : Int {
    var marble = keptMarbles.set(curPlayer, null) ?: Marble()

    if(marble.num % 23 == 0) {
        var score = marble.num
        marble = curMarble
        for (i in 0..6) {
            marble = marble.left
        }

        keptMarbles[curPlayer] = marble.removeMarble()
        score += marble.num
        curMarble = marble.right

        players[curPlayer] += score
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