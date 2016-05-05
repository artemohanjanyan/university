package test;

import static expression.Util.checkAssert;

/**
 * @author Niyaz Nigmatullin
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ModifiedParserTest {
    public static void main(final String[] args) {
        checkAssert(ModifiedParserTest.class);
        new ModifiedExpressionTest(true).test();
    }
}
