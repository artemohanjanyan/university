package ru.ifmo.mpp.hashmap;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

/**
 * @author Nikita Koval.
 */
public class IntIntHashMapStressTest {

    private static final int N = 1_000_000;
    private static final int MAX_KEY = 100;
    private static final int MAX_VALUE = 500;
    private static final Random RAND = new Random(0);

    private IntIntHashMap map = new IntIntHashMap();

    @Test
    public void test() {
        Map<Integer, Integer> expectedMap = new HashMap<>();
        for (int i = 0; i < N; i++) {
            int key = RAND.nextInt(MAX_KEY) + 1;
            int val = RAND.nextInt(MAX_VALUE) + 1;
            if (RAND.nextBoolean()) {
                Integer expected = expectedMap.put(key, val);
                if (expected == null)
                    expected = 0;
                assertEquals((int) expected, map.put(key, val));
            } else {
                Integer expected = expectedMap.remove(key);
                if (expected == null)
                    expected = 0;
                assertEquals((int) expected, map.remove(key));
            }
            for (key = 1; key <= MAX_KEY; key++) {
                Integer expected = expectedMap.get(key);
                if (expected == null)
                    expected = 0;
                assertEquals((int) expected, map.get(key));
            }
        }
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