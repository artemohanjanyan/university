package number;

import expression.parser.AbstractNumberReader;
import expression.parser.IllegalTokenException;

public class DoubleReader extends AbstractNumberReader<DoubleNumber> {
    @Override
    public int skip(String s, int i) {
        while (i < s.length() && Character.isDigit(s.charAt(i))) {
            ++i;
        }

        if (i < s.length() && s.charAt(i) == '.') {
            ++i;
            if (i < s.length() && Character.isDigit(s.charAt(i))) {
                while (i < s.length() && Character.isDigit(s.charAt(i))) {
                    ++i;
                }
            } else {
                --i;
            }
        }

        return i;
    }

    @Override
    public boolean assume(char c) {
        return Character.isDigit(c) || c == '.';
    }

    @Override
    public DoubleNumber create(String s) throws IllegalTokenException {
        return new DoubleNumber(s);
    }

}
