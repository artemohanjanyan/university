package aohanjanyan.se

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class LinkedListTest {
    private fun initData(): Pair<LinkedList<Int>, List<LinkedList.Pointer<Int>>> {
        val list = LinkedList<Int>()
        val pointers = ArrayList<LinkedList.Pointer<Int>>()
        for (i in 0 until 5) {
            pointers.add(list.addToHead(i))
        }
        return Pair(list, pointers)
    }

    @Test
    fun emptyHeadTailTest() {
        val list = LinkedList<Int>()
        assertNull(list.head())
        assertNull(list.tail())
    }

    @Test
    fun addToHeadTest() {
        val list = LinkedList<Int>()
        assertEquals(0, list.addToHead(0).value())
        assertEquals(1, list.addToHead(1).value())
        assertEquals(2, list.addToHead(2).value())
        assertEquals(0, list.tail()!!.value())
        assertEquals(2, list.head()!!.value())
    }

    @Test
    fun sizeTest() {
        val (list, _) = initData()
        assertEquals(5, list.size())
        list.addToHead(-1)
        assertEquals(6, list.size())
        list.tail()!!.remove()
        assertEquals(5, list.size())
    }

    @Test
    fun inflateAndClearTest() {
        val list = LinkedList<Int>()
        assertNull(list.head())
        assertNull(list.tail())
        for (i in 0 until 5) {
            list.addToHead(i)
        }
        assertNotNull(list.head())
        assertNotNull(list.tail())
        for (i in 0 until 2) {
            list.head()!!.remove()
            list.tail()!!.remove()
        }
        assertTrue(list.head()!!.value() == list.tail()!!.value())
        list.head()!!.remove()
        assertNull(list.head())
        assertNull(list.tail())
        list.addToHead(0).remove()
        assertNull(list.head())
        assertNull(list.tail())
    }

    @Test
    fun moveToHeadTest() {
        val (list, pointers) = initData()
        for (pointer in pointers) {
            pointer.moveToHead()
        }
        assertEquals(5, list.size())
        assertEquals(4, list.head()!!.value())
        assertEquals(0, list.tail()!!.value())
        for (pointer in pointers.reversed()) {
            pointer.moveToHead()
        }
        assertEquals(5, list.size())
        assertEquals(0, list.head()!!.value())
        assertEquals(4, list.tail()!!.value())
    }
}