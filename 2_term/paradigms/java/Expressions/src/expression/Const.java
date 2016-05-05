package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Const extends AbstractValue {
    public Const(int value) {
        intValue = () -> value;
        doubleValue = () -> (double) value;
    }
}
