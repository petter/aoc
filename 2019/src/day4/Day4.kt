package day4

fun main() {
    val from = 245182
    val to = 790572

    val passwords1 = validPasswords(from, to, listOf(::nonDecreasingConstraint, ::sameAdjacentSiblingConstraint))
    val passwords2 = validPasswords(from, to, listOf(::nonDecreasingConstraint, ::sameAdjacentSiblingConstraint2))
    println("Amount of valid passwords: ${passwords1.size}")
    println("Amount of valid passwords 2: ${passwords2.size}")
}

fun validPasswords(from: Int, to: Int, constraints: List<(String) -> Boolean>) : List<Int> {
    val res = mutableListOf<Int>()
     for (i in from..to) {
        val password = i.toString()
         if(constraints.all { it(password) })
            res.add(i)
     }
    return res
}

fun nonDecreasingConstraint(password: String) : Boolean {
    for (i in password.indices.toList().dropLast(1)) {
        if (password[i] > password[i + 1]) return false
    }
    return true
}

fun sameAdjacentSiblingConstraint(password: String) : Boolean {
    return Regex("(.)\\1").containsMatchIn(password)
}

fun sameAdjacentSiblingConstraint2(password: String) : Boolean {
    return sameAdjacentSiblingConstraint(Regex("((.)\\2{2,})").replace(password, ""))
}
