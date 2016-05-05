package number;

import expression.parser.IllegalTokenException;

import java.util.function.BiFunction;

public class DoubleNumber implements Number<DoubleNumber> {
    private final double x;

    public DoubleNumber(double x) {
        this.x = x;
    }

    public DoubleNumber(String str) throws IllegalTokenException {
        try {
            this.x = Double.parseDouble(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    @Override
    public DoubleNumber add(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> a + b, "+");
    }

    @Override
    public DoubleNumber subtract(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> a - b, "-");
    }

    @Override
    public DoubleNumber multiply(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> a * b, "*");
    }

    @Override
    public DoubleNumber divide(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> a / b, "/");
    }

    @Override
    public DoubleNumber modulo(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> a % b, "%");
    }

    @Override
    public DoubleNumber negate() {
        return ZERO.subtract(this);
    }

    @Override
    public DoubleNumber abs() {
        return new DoubleNumber(Math.abs(x));
    }

    public DoubleNumber sqrt() {
        if (Double.isNaN(Math.sqrt(x))) {
            throw new IllegalArgumentException("sqrt", this);
        }
        return new DoubleNumber(Math.sqrt(x));
    }

    public DoubleNumber pow(DoubleNumber that) {
        return checkedOperation(that, Math::pow, "**");
    }

    public DoubleNumber log(DoubleNumber that) {
        return checkedOperation(that, (a, b) -> Math.log(a) / Math.log(b), "//");
    }

    @Override
    public int compareTo(DoubleNumber that) {
        return Double.compare(this.x, that.x);
    }

    public static final DoubleNumber ZERO = new DoubleNumber(0);

    public double getX() {
        return x;
    }

    @Override
    public String toString() {
        return Double.toString(x);
    }

    private DoubleNumber checkedOperation(DoubleNumber that, BiFunction<Double, Double, Double> function, String str)
            throws OverflowException {
        Double ans = function.apply(this.x, that.x);
//        if (Double.isInfinite(ans)) {
//            throw new OverflowException(this, str, that);
//        } else if (Double.isNaN(ans)) {
//            throw new IllegalArgumentException(this, str, that);
//        }
        return new DoubleNumber(ans);
    }

    @Override
    public DoubleNumber square() {
        return this.multiply(this);
    }


}
