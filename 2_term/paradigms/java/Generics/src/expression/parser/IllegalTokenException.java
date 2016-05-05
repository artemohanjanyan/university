package expression.parser;

public class IllegalTokenException extends ParserException {
    public IllegalTokenException(String str) {
        super("illegal token " + str);
    }

    public IllegalTokenException(String str, Throwable cause) {
        super("illegal token " + str, cause);
    }
}
