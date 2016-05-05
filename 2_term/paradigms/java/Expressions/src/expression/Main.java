package expression;

/**
 * Created by artem on 10/03/15.
 */
public class Main {
    public static void main(String[] args) {
        AbstractExpression e =
                new Add(
                        new Subtract(
                                new Multiply(
                                        new Variable("x"),
                                        new Variable("x")
                                ),
                                new Multiply(
                                        new Const(2),
                                        new Variable("x")
                                )
                        ),
                        new Const(1)
                );

        System.out.println(e.evaluate(Integer.parseInt(args[0])));
    }
}
