package expression;

import number.Number;

import java.util.HashMap;

/**
 * Created by artem on 10/03/15.
 */
public class VariableContainer {
    private HashMap<String, number.Number> map;

    public VariableContainer() {
        map = new HashMap<>();
    }

    public VariableContainer(String name, Number value) {
        this();
        set(name, value);
    }

    public VariableContainer(String name, Integer value) {
        this(name, new Number(value));
    }

    public void set(String name, Number value) {
        map.put(name, value);
    }

    public void set(String name, int value) {
        map.put(name, new Number(value));
    }

    public Number get(String name) {
        return map.get(name);
    }
}
