package number;

public class DivisionByZeroException extends NumberOperationException {
    public DivisionByZeroException(Number dividend) {
        this(dividend, null);
    }

    public DivisionByZeroException(Number dividend, Throwable cause) {
        super(dividend, "/", Number.ZERO, cause);
    }
}
