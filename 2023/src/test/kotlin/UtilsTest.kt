import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

internal class UtilsTest {

    @Test
    fun toPairWorksForListsWithTwoMembers() {
        val expected = Pair(1, 2)
        val actual = listOf(1, 2).toPair()
        assertEquals(expected, actual)
    }

    @Test
    fun toPairThrowsErrorWithInvalidAmountOfMembers() {
        assertThrows(IllegalArgumentException::class.java) {
            listOf(1, 2, 3).toPair()
        }
    }

    @Test
    fun generatePointsBetweenWorksWithIncreasingY() {
        val expected = listOf(
                Pair(0, 0),
                Pair(0, 1),
                Pair(0, 2)
        )
        val actual = generatePointsBetween(Pair(0, 0), Pair(0, 2))
        assertEquals(expected, actual)
    }

    @Test
    fun generatePointsBetweenWorksWithIncreasingX() {
        val expected = listOf(
            Pair(1, 0),
            Pair(2, 0),
            Pair(3, 0),
        )
        val actual = generatePointsBetween(Pair(1, 0), Pair(3, 0))
        assertEquals(expected, actual)
    }

    @Test
    fun generatePointsBetweenWorksWithDecreasingY() {
        val expected = listOf(
            Pair(0, 0),
            Pair(0, -1),
            Pair(0, -2)
        )
        val actual = generatePointsBetween(Pair(0, 0), Pair(0, -2))
        assertEquals(expected, actual)
    }

    @Test
    fun generatePointsBetweenWorksWithDecreasingX() {
        val expected = listOf(
            Pair(-1, 0),
            Pair(-2, 0),
            Pair(-3, 0),
        )
        val actual = generatePointsBetween(Pair(-1, 0), Pair(-3, 0))
        assertEquals(expected, actual)
    }

    @Test
    fun generatePointsBetweenWorksWith45DegreeDiagonals() {
        val expected = listOf(
            Pair(1, 1),
            Pair(2, 2),
            Pair(3, 3),
        )
        val actual = generatePointsBetween(Pair(1, 1), Pair(3, 3))
        assertEquals(expected, actual)
    }

}