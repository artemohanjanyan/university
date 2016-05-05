package expression;

import java.util.HashMap;

/**
 * Created by artem on 10/03/15.
 */
public class VariableContainer {
    private HashMap<String, Integer> map;

    public VariableContainer() {
        map = new HashMap<>();
    }

    public VariableContainer(String name, Integer value) {
        this();
        set(name, value);
    }

    public void set(String name, Integer value) {
        map.put(name, value);
    }

    public Integer get(String name) {
        return map.get(name);
    }
}
