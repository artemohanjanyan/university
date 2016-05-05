package number;

public class NumberOperationException extends RuntimeException {
    public NumberOperationException(String operation, Number<?> argument) {
        this(operation, argument, null);
    }

    public NumberOperationException(String operation, Number<?> argument, Throwable cause) {
        super(operation + " " + argument, cause);
    }

    public NumberOperationException(Number<?> left, String operation, Number<?> right) {
        this(left, operation, right, null);
    }

    public NumberOperationException(Number<?> left, String operation, Number<?> right, Throwable cause) {
        super(left + " " + operation + " " + right, cause);
    }
}
