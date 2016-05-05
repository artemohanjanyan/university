package expression;

/**
 * Created by artem on 10/03/15.
 */
public class VariableNotDefinedException extends Exception {
    public VariableNotDefinedException() {
        super();
    }

    public VariableNotDefinedException(String variableName) {
        super("Variable '" + variableName + "' is not defined!");
    }
}
