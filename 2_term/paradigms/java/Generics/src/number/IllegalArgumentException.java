package number;

public class IllegalArgumentException extends NumberOperationException {
    public IllegalArgumentException(String operation, Number<?> argument) {
        super(operation, argument);
    }

    public IllegalArgumentException(String operation, Number<?> argument, Throwable cause) {
        super(operation, argument, cause);
    }

    public IllegalArgumentException(Number<?> left, String operation, Number<?> right) {
        super(left, operation, right);
    }

    public IllegalArgumentException(Number<?> left, String operation, Number<?> right, Throwable cause) {
        super(left, operation, right, cause);
    }
}
