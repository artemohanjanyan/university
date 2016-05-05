package expression;

import java.util.function.BiFunction;

/**
 * Created by Artem Ohanjanyan on 24/03/15.
 */
public class RightShift extends AbstractBinaryOperation {
    public RightShift(AbstractExpression left, AbstractExpression right) {
        super(left, right, (x, y) -> x >> y);
    }
}
