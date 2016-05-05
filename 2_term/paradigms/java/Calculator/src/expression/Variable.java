package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Variable extends AbstractValue {
    public Variable(String name) {
        super(() -> variables.get(name));
    }
}
