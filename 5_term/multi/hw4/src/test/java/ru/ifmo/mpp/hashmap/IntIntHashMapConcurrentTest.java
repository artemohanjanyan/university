package ru.ifmo.mpp.hashmap;

import com.devexperts.dxlab.lincheck.Checker;
import com.devexperts.dxlab.lincheck.annotations.CTest;
import com.devexperts.dxlab.lincheck.annotations.Operation;
import com.devexperts.dxlab.lincheck.annotations.ReadOnly;
import com.devexperts.dxlab.lincheck.annotations.Reload;
import com.devexperts.dxlab.lincheck.util.Result;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

/**
 * @author Nikita Koval.
 */
@CTest(iter = 100, actorsPerThread = {"1:5", "1:5"})
@CTest(iter = 100, actorsPerThread = {"1:5", "1:5", "1:5"})
@CTest(iter = 100, actorsPerThread = {"1:3", "1:3", "1:3", "1:3"})
public class IntIntHashMapConcurrentTest {

    private IntIntHashMap map;

    @Reload
    public void reload() {
        map = new IntIntHashMap();
    }

    @Operation(args = {"1:5", "1:10"})
    public void put(Result res, Object[] args) throws Exception {
        Integer key = (Integer) args[0];
        Integer value = (Integer) args[1];
        res.setValue(map.put(key, value));
    }

    @Operation(args = {"1:5"})
    public void remove(Result res, Object[] args) throws Exception {
        Integer key = (Integer) args[0];
        res.setValue(map.remove(key));
    }

    @ReadOnly
    @Operation(args = {"1:5"})
    public void get(Result res, Object[] args) throws Exception {
        Integer key = (Integer) args[0];
        res.setValue(map.get(key));
    }

    @Test
    public void test() throws Exception {
        assertTrue(Checker.check(new IntIntHashMapConcurrentTest()));
    }
}
