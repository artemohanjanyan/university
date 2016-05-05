package test;

import static expression.Util.checkAssert;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ParserTest {
    public static void main(final String[] args) {
        checkAssert(ParserTest.class);
        new ExpressionTest(true).test();
    }
}
