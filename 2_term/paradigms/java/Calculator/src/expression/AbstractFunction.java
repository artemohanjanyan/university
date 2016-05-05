package expression;

import java.util.function.Function;

/**
 * Created by Artem Ohanjanyan on 20/03/15.
 */
public class AbstractFunction extends AbstractExpression {
    private AbstractExpression argument;
    private Function<Integer, Integer> function;

    public AbstractFunction(AbstractExpression argument, Function<Integer, Integer> function) {
        this.argument = argument;
        this.function = function;
    }

    @Override
    protected int evaluate() {
        return function.apply(argument.evaluate());
    }

    @Override
    public String toString() {
        return super.toString() + "(" + argument.toString() + ")";
    }
}
