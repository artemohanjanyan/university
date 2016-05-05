package expression;

import java.util.function.BiFunction;

/**
 * Created by Artem Ohanjanyan on 24/03/15.
 */
public class LeftShift extends AbstractBinaryOperation {
    public LeftShift(AbstractExpression left, AbstractExpression right) {
        super(left, right, (x, y) -> x << y);
    }
}
