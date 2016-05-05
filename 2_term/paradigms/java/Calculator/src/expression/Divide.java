package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Divide extends AbstractBinaryOperation {
    public Divide(AbstractExpression a, AbstractExpression b) {
        super(a, b, (x, y) -> x / y);
    }
}
