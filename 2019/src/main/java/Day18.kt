import java.io.File

typealias Position = Pair<Int, Int>

fun main() {

    val input = File("in/day18.txt").readText()
    val maze = Maze.fromString(input) { if(it == '#') 0 else 1 }
    println(maze)
}



