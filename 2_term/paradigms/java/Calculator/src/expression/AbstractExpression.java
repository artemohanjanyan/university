package expression;

/**
 * Created by artem on 10/03/15.
 */
public abstract class AbstractExpression
        implements Expression, TripleExpression {
    protected static VariableContainer variables;

    abstract protected int evaluate();

    @Override
    public int evaluate(int xValue) {
        variables = new VariableContainer("x", xValue);
        return evaluate();
    }

    @Override
    public int evaluate(int x, int y, int z) {
        variables = new VariableContainer();
        variables.set("x", x);
        variables.set("y", y);
        variables.set("z", z);
        return evaluate();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
