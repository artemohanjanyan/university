package aohanjanyan.cache

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class WeightedLruCacheTest {

    @Test
    fun initTest() {
        assertThrows(IllegalArgumentException::class.java, {
            WeightedLruCache<Int, Int>(0)
        })
    }

    @Test
    fun cacheTest() {
        val cache: Cache<Int, Int> = WeightedLruCache(5)
        for (i in 1..5) {
            cache.put(i, i)
            assertEquals(i, cache.get(i))
        }
        cache.put(6, 6)
        assertEquals(6, cache.get(6))
        assertNull(cache.get(1))
        assertEquals(2, cache.get(2))
        cache.put(1, 1)
        assertNull(cache.get(3))
        cache.put(1, 2)
        assertEquals(2, cache.get(1))
        assertEquals(2, cache.get(2))
        assertEquals(4, cache.get(4))
        assertEquals(5, cache.get(5))
        assertEquals(6, cache.get(6))
    }

    @Test
    fun tooFatTest() {
        val cache: WeightedCache<Int, Int> = WeightedLruCache(10)
        assertThrows(IllegalArgumentException::class.java, {
            cache.put(11, 11, 11)
        })
    }

    @Test
    fun weightedCacheTest() {
        val cache: WeightedCache<Int, Int> = WeightedLruCache(10)
        for (i in 1..4) {
            cache.put(i, i, i)
            assertEquals(i, cache.get(i))
        }
        cache.put(5, 5, 5)
        assertNull(cache.get(1))
        assertNull(cache.get(2))
        assertNull(cache.get(3))
        cache.put(6, 6, 6)
        assertNull(cache.get(4))
        assertNull(cache.get(5))
        cache.put(7, 7, 7)
        assertNull(cache.get(6))
        cache.put(4, 4, 4)
        cache.put(5, 5, 5)
        cache.put(4, 4, 6)
        assertNull(cache.get(5))
        assertEquals(4, cache.get(4))
        cache.put(5, 5, 5)
        assertNull(cache.get(4))
    }
}