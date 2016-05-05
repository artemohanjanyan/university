package number;

import expression.parser.AbstractNumberReader;
import expression.parser.IllegalTokenException;

/**
 * Created by Artem Ohanjanyan on 05/04/15.
 */
public class BigIntegerReader extends AbstractNumberReader<BigIntegerNumber> {
    @Override
    public int skip(String s, int i) {
        while (i < s.length() && Character.isDigit(s.charAt(i))) {
            ++i;
        }
        return i;
    }

    @Override
    public boolean assume(char c) {
        return Character.isDigit(c);
    }

    @Override
    public BigIntegerNumber create(String s) throws IllegalTokenException {
        return new BigIntegerNumber(s);
    }
}
