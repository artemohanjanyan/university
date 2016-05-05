package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Variable extends AbstractValue {
    private String name;

    public Variable(String name) {
        this.name = name;

        intValue = () -> intVariables.getValue(this.name);
        doubleValue = () -> doubleVariables.getValue(this.name);
    }
}
