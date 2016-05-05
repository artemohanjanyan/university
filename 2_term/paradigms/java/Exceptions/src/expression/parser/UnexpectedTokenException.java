package expression.parser;

/**
 * Created by Artem Ohanjanyan on 31/03/15.
 */
public class UnexpectedTokenException extends ParserException {
    public UnexpectedTokenException(CheckedParser.TokenType expected, CheckedParser.TokenType found) {
        this(expected.toString(), found);
    }

    public UnexpectedTokenException(String expected, CheckedParser.TokenType found) {
        this(expected, found, null);
    }

    public UnexpectedTokenException(String expected, CheckedParser.TokenType found, Throwable cause) {
        super(expected + " expected, but " + found.toString() + " found", cause);
    }
}
