package expression;

import static expression.Util.assertEquals;
import static expression.Util.checkAssert;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ExpressionParserTest {
    public static void main(final String[] args) {
        checkAssert(ExpressionParserTest.class);
        testExpression("10", (x, y, z) -> 10);
        testExpression("x", (x, y, z) -> x);
        testExpression("y", (x, y, z) -> y);
        testExpression("z", (x, y, z) -> z);
        testExpression("x+2", (x, y, z) -> x + 2);
        testExpression("2-y", (x, y, z) -> 2 - y);
        testExpression("3*z", (x, y, z) -> 3 * z);
        testExpression("x/-2", (x, y, z) -> -x / 2);
        testExpression(
                "x*y+(z-1)/10",
                (x, y, z) -> x * y + (z - 1) / 10
        );
        testExpression(
                "x*abs(y+(z-1)/10)",
                (x, y, z) -> x * Math.abs(y + (z - 1) / 10)
        );
        System.out.println("OK");
    }

    private static void testExpression(final String description, final TripleExpression expected) {
        Parser expressionParser = new ExpressionParser();
        System.out.println("Testing " + description);
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                for (int k = 0; k < 10; k++) {
                    assertEquals(String.format("f(%d, %d, %d)", i, j, k),
                            expressionParser.parse(description).evaluate(i, j, k), expected.evaluate(i, j, k));
                }
            }
        }
    }
}