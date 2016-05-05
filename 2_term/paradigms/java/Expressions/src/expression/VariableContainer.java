package expression;

import java.util.HashMap;

/**
 * Created by artem on 10/03/15.
 */
public class VariableContainer<T> {
    private HashMap<String, T> map;

    public VariableContainer() {
        map = new HashMap<String, T>();
    }

    public VariableContainer(String name, T value) {
        this();
        set(name, value);
    }

    public void set(String name, T value) {
        map.put(name, value);
    }

    public T getValue(String name) {
//        Double value = map.get(name);
//
//        if (value == null) {
//            throw new VariableNotDefinedException(name);
//        } else {
//            return value;
//        }

        return map.get(name);
    }
}
