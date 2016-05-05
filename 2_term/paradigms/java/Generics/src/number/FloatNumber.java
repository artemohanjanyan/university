package number;

import expression.parser.IllegalTokenException;

import java.util.function.BiFunction;

public class FloatNumber implements Number<FloatNumber> {
    private final float x;

    public FloatNumber(float x) {
        this.x = x;
    }

    public FloatNumber(String str) throws IllegalTokenException {
        try {
            this.x = Float.parseFloat(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    @Override
    public FloatNumber add(FloatNumber that) {
        return checkedOperation(that, (a, b) -> a + b, "+");
    }

    @Override
    public FloatNumber subtract(FloatNumber that) {
        return checkedOperation(that, (a, b) -> a - b, "-");
    }

    @Override
    public FloatNumber multiply(FloatNumber that) {
        return checkedOperation(that, (a, b) -> a * b, "*");
    }

    @Override
    public FloatNumber divide(FloatNumber that) {
        return checkedOperation(that, (a, b) -> a / b, "/");
    }

    @Override
    public FloatNumber modulo(FloatNumber that) {
        return checkedOperation(that, (a, b) -> a % b, "%");
    }

    @Override
    public FloatNumber negate() {
        return ZERO.subtract(this);
    }

    public static final FloatNumber ZERO = new FloatNumber(0);

    @Override
    public FloatNumber abs() {
        return new FloatNumber(Math.abs(x));
    }

    @Override
    public int compareTo(FloatNumber that) {
        return Float.compare(this.x, that.x);
    }

    private FloatNumber checkedOperation(FloatNumber that, BiFunction<Float, Float, Float> function, String str)
            throws OverflowException {
        Float ans = function.apply(this.x, that.x);
//        if (Double.isInfinite(ans)) {
//            throw new OverflowException(this, str, that);
//        } else if (Double.isNaN(ans)) {
//            throw new IllegalArgumentException(this, str, that);
//        }
        return new FloatNumber(ans);
    }

    public float getX() {
        return x;
    }

    @Override
    public FloatNumber square() {
        return this.multiply(this);
    }
}
