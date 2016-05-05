package number;

import expression.parser.AbstractNumberReader;
import expression.parser.IllegalTokenException;

public class UnsafeIntegerReader extends AbstractNumberReader<UnsafeIntegerNumber> {
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
    public UnsafeIntegerNumber create(String s) throws IllegalTokenException {
        return new UnsafeIntegerNumber(s);
    }
}
