package expression;

import java.util.function.Function;

/**
 * Created by Artem Ohanjanyan on 20/03/15.
 */
public class Abs extends AbstractFunction {
    public Abs(AbstractExpression argument) {
        super(argument, Math::abs);
    }
}
