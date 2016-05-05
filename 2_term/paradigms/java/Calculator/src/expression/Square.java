package expression;

import java.util.function.Function;

/**
 * Created by Artem Ohanjanyan on 24/03/15.
 */
public class Square extends AbstractFunction {
    public Square(AbstractExpression argument) {
        super(argument, (x) -> x * x);
    }
}
