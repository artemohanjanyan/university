package expression;

import number.Number;

import java.util.HashMap;

public class VariableContainer<T extends Number<T>> {
    private HashMap<String, T> map;

    public VariableContainer() {
        map = new HashMap<>();
    }

    public VariableContainer<T> set(String name, T value) {
        map.put(name, value);
        return this;
    }

    public T get(String name) {
        return map.get(name);
    }
}
