package day4

fun main() {
    val from = 245182
    val to = 790572

    val passwords = validPasswords(from, to)
    println("Amount of valid passwords: ${passwords.size}")
}

fun validPasswords(from: Int, to: Int) : List<Int> {
    val res = mutableListOf<Int>()
     for (i in from..to) {
        val password = i.toString()
        if (nonDecreasingConstraint(password) && sameAdjacentSiblingConstraint(password))
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
