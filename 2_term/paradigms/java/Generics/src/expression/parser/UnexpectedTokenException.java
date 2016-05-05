package expression.parser;

public class UnexpectedTokenException extends ParserException {
    public UnexpectedTokenException(Parser.TokenType expected, Parser.TokenType found) {
        this(expected.toString(), found);
    }

    public UnexpectedTokenException(String expected, Parser.TokenType found) {
        this(expected, found, null);
    }

    public UnexpectedTokenException(String expected, Parser.TokenType found, Throwable cause) {
        super(expected + " expected, but " + found.toString() + " found", cause);
    }
}
