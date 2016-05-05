package expression;

/**
 * Created by Artem Ohanjanyan on 24/03/15.
 */
public class Mod extends AbstractBinaryOperation {
    public Mod(AbstractExpression left, AbstractExpression right) {
        super(left, right, (x, y) -> x % y);
    }
}
