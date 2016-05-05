package number;

import expression.parser.IllegalTokenException;
import expression.parser.AbstractNumberReader;

public class IntegerReader extends AbstractNumberReader<IntegerNumber> {
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
    public IntegerNumber create(String s) throws IllegalTokenException {
        return new IntegerNumber(s);
    }
}