package number;

import expression.parser.IllegalTokenException;

public class UnsafeIntegerNumber implements Number<UnsafeIntegerNumber> {
    private final int n;

    public UnsafeIntegerNumber(int n) {
        this.n = n;
    }

    public UnsafeIntegerNumber(String str) throws IllegalTokenException {
        try {
            this.n = Integer.parseInt(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    @Override
    public UnsafeIntegerNumber add(UnsafeIntegerNumber that) {
        return new UnsafeIntegerNumber(this.n + that.n);
    }

    @Override
    public UnsafeIntegerNumber subtract(UnsafeIntegerNumber that) {
        return new UnsafeIntegerNumber(this.n - that.n);
    }

    @Override
    public UnsafeIntegerNumber multiply(UnsafeIntegerNumber that) {
        return new UnsafeIntegerNumber(this.n * that.n);
    }

    @Override
    public UnsafeIntegerNumber divide(UnsafeIntegerNumber that) {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "/", that);
        }
        return new UnsafeIntegerNumber(this.n / that.n);
    }

    @Override
    public UnsafeIntegerNumber modulo(UnsafeIntegerNumber that) {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "%", that);
        }
        return new UnsafeIntegerNumber(this.n % that.n);
    }

    @Override
    public UnsafeIntegerNumber negate() {
        return new UnsafeIntegerNumber(-this.n);
    }

    @Override
    public UnsafeIntegerNumber abs() {
        if (n > 0) {
            return this;
        } else {
            return this.negate();
        }
    }

    @Override
    public int compareTo(UnsafeIntegerNumber that) {
        return Integer.compare(this.n, that.n);
    }

    public int getN() {
        return n;
    }

    @Override
    public UnsafeIntegerNumber square() {
        return this.multiply(this);
    }
}
