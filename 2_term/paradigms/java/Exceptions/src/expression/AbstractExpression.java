package expression;

import number.NumberOperationException;

/**
 * Created by artem on 10/03/15.
 */
public abstract class AbstractExpression
        implements TripleExpression {
    protected static VariableContainer variables;

    abstract protected number.Number evaluate() throws NumberOperationException;

    @Override
    public int evaluate(int x, int y, int z) throws NumberOperationException {
        variables = new VariableContainer();
        variables.set("x", x);
        variables.set("y", y);
        variables.set("z", z);
        return evaluate().getN();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
