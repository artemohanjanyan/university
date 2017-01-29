package ru.ifmo.mpp.hashmap;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * @author Roman Elizarov.
 */
public class IntIntHashMapTest {
    private IntIntHashMap map = new IntIntHashMap();

    @Test
    public void testSimple() {
        assertThat(map.get(1), is(0));
        assertThat(map.put(1, 42), is(0));
        assertThat(map.get(1), is(42));
        assertThat(map.get(1), is(42));
        assertThat(map.remove(1), is(42));
        assertThat(map.get(1), is(0));
    }

    @Test
    public void testRehash() {
        int n = 1000;
        // check all are zero
        for (int i = 1; i <= n; i++) {
            assertThat(map.get(i), is(0));
        }
        // put & check all
        for (int i = 1; i <= n; i++) {
            assertThat(map.get(i), is(0));
            assertThat(map.put(i, valOf(i)), is(0));
            assertThat(map.get(i), is(valOf(i)));
        }
        // check all again
        for (int i = 1; i <= n; i++) {
            assertThat(map.get(i), is(valOf(i)));
        }
        // remove all
        for (int i = 1; i <= n; i++) {
            assertThat(map.remove(i), is(valOf(i)));
            assertThat(map.get(i), is(0));
        }
        // check all again
        for (int i = 1; i <= n; i++) {
            assertThat(map.get(i), is(0));
        }
    }

    private int valOf(int i) {
        return i * i;
    }
}