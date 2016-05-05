package expression;

/**
 * Created by artem on 10/03/15.
 */
public abstract class AbstractExpression
        implements Expression, DoubleExpression, TripleExpression {
    protected static IntegerVariableContainer intVariables;
    protected static DoubleVariableContainer doubleVariables;

    abstract protected int intEvaluate();
    abstract protected double doubleEvaluate();

    @Override
    public int evaluate(int xValue) {
        intVariables = new IntegerVariableContainer("x", xValue);
        return intEvaluate();
    }

    @Override
    public double evaluate(double x) {
        doubleVariables = new DoubleVariableContainer("x", x);
        return doubleEvaluate();
    }

    @Override
    public int evaluate(int x, int y, int z) {
        intVariables = new IntegerVariableContainer();
        intVariables.set("x", x);
        intVariables.set("y", y);
        intVariables.set("z", z);
        return intEvaluate();
    }
}
