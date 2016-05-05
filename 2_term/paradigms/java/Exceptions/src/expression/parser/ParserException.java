package expression.parser;

/**
 * Created by Artem Ohanjanyan on 26/03/15.
 */
public class ParserException extends Exception {
    public ParserException(String message) {
        super(message);
    }

    public ParserException(String message, Throwable cause) {
        super(message, cause);
    }
}
