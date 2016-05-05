package expression.parser;

/**
 * Created by Artem Ohanjanyan on 26/03/15.
 */
public class IllegalTokenException extends ParserException {
    public IllegalTokenException(String str) {
        super("illegal token " + str);
    }

    public IllegalTokenException(String str, Throwable cause) {
        super("illegal token " + str, cause);
    }
}
