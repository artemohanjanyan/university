package aohanjanyan.twitter

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class SimpleDiagramKtTest {

    private val expected = listOf(1, 0, 2, 0, 1)

    @Test
    fun simpleTest() {
        assertEquals(expected, makeDiagram(listOf(0, 2, 2, 4), 5).asList())
    }

    @Test
    fun emptyTest() {
        assertEquals(listOf(0, 0, 0, 0, 0), makeDiagram(listOf(), 5).asList())
    }

    @Test
    fun badHoursTest() {
        assertEquals(
                expected,
                makeDiagram(listOf(-1, 2, 0, 2, 4, 5), 5).asList())
    }
}