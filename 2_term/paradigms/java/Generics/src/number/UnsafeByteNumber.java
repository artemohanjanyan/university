package number;

import expression.parser.IllegalTokenException;

public class UnsafeByteNumber implements Number<UnsafeByteNumber> {
    private final byte n;

    public UnsafeByteNumber(int n) {
        this.n = (byte) n;
    }

    public UnsafeByteNumber(String str) throws IllegalTokenException {
        try {
            this.n = Byte.parseByte(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    @Override
    public UnsafeByteNumber add(UnsafeByteNumber that) {
        return new UnsafeByteNumber(this.n + that.n);
    }

    @Override
    public UnsafeByteNumber subtract(UnsafeByteNumber that) {
        return new UnsafeByteNumber(this.n - that.n);
    }

    @Override
    public UnsafeByteNumber multiply(UnsafeByteNumber that) {
        return new UnsafeByteNumber(this.n * that.n);
    }

    @Override
    public UnsafeByteNumber divide(UnsafeByteNumber that) {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "/", that);
        }
        return new UnsafeByteNumber(this.n / that.n);
    }

    @Override
    public UnsafeByteNumber modulo(UnsafeByteNumber that) {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "%", that);
        }
        return new UnsafeByteNumber(this.n % that.n);
    }

    @Override
    public UnsafeByteNumber negate() {
        return new UnsafeByteNumber(-this.n);
    }

    @Override
    public UnsafeByteNumber abs() {
        if (n > 0) {
            return this;
        } else {
            return this.negate();
        }
    }

    @Override
    public int compareTo(UnsafeByteNumber that) {
        return Byte.compare(this.n, that.n);
    }

    public byte getN() {
        return n;
    }

    @Override
    public UnsafeByteNumber square() {
        return this.multiply(this);
    }
}
