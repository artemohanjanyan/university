package number;

import expression.parser.AbstractNumberReader;
import expression.parser.IllegalTokenException;

public class FloatReader extends AbstractNumberReader<FloatNumber> {
    @Override
    public int skip(String s, int i) {
        while (i < s.length() && Character.isDigit(s.charAt(i))) {
            ++i;
        }

        i = getI(s, i);

        return i;
    }

    private int getI(String s, int i) {
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
    public FloatNumber create(String s) throws IllegalTokenException {
        return new FloatNumber(s);
    }

}
