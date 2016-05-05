package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Subtract extends AbstractBinaryOperation {
    public Subtract(AbstractExpression a, AbstractExpression b) {
        super(a, b, (x, y) -> x - y, (x, y) -> x - y);
    }
}
