package number;

public class OverflowException extends NumberOperationException {
    public OverflowException(String operation, Number argument) {
        super(operation, argument);
    }

    public OverflowException(String operation, Number argument, Throwable cause) {
        super(operation, argument, cause);
    }

    public OverflowException(Number left, String operation, Number right) {
        super(left, operation, right);
    }

    public OverflowException(Number left, String operation, Number right, Throwable cause) {
        super(left, operation, right, cause);
    }
}
