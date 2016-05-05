package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Add extends AbstractBinaryOperation {
    public Add(AbstractExpression a, AbstractExpression b) {
        super(a, b, (x, y) -> x + y, (x, y) -> x + y);
    }
}
