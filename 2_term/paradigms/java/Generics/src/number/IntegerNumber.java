package number;

import expression.parser.IllegalTokenException;

import java.util.function.Predicate;

public class IntegerNumber implements Number<IntegerNumber> {
    private final int n;

    public IntegerNumber(int n) {
        this.n = n;
    }

    public IntegerNumber(String str) throws IllegalTokenException {
        try {
            this.n = Integer.parseInt(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    @Override
    public IntegerNumber add(IntegerNumber that) throws OverflowException {
        if (that.n > 0 && Integer.MAX_VALUE - that.n < this.n ||
                that.n < 0 && Integer.MIN_VALUE - that.n > this.n) {
            throw new OverflowException(this, "+", that);
        }
        return new IntegerNumber(this.n + that.n);
    }

    @Override
    public IntegerNumber subtract(IntegerNumber that) throws OverflowException {
        if (that.n < 0 && Integer.MAX_VALUE + that.n < this.n ||
                that.n > 0 && Integer.MIN_VALUE + that.n > this.n) {
            throw new OverflowException(this, "-", that);
        }
        return new IntegerNumber(this.n - that.n);
    }

    @Override
    public IntegerNumber multiply(IntegerNumber that) throws OverflowException {
        if (this.n > 0 && that.n > 0 && Integer.MAX_VALUE / that.n < this.n ||
                this.n > 0 && that.n < 0 && Integer.MIN_VALUE / this.n > that.n ||
                this.n < 0 && that.n > 0 && Integer.MIN_VALUE / that.n > this.n ||
                this.n < 0 && that.n < 0 && Integer.MAX_VALUE / that.n > this.n) {
            throw new OverflowException(this, "*", that);
        }
        return new IntegerNumber(this.n * that.n);
    }

    @Override
    public IntegerNumber divide(IntegerNumber that) throws NumberOperationException {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "/", that);
        }
        if (this.n == Integer.MIN_VALUE && that.n == -1) {
            throw new OverflowException(this, "/", that);
        }
        return new IntegerNumber(this.n / that.n);
    }

    public IntegerNumber modulo(IntegerNumber that) throws IllegalArgumentException {
        if (that.n == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "%", that);
        }
        return new IntegerNumber(this.n % that.n);
    }

    @Override
    public IntegerNumber negate() throws OverflowException {
        if (n == Integer.MIN_VALUE) {
            throw new OverflowException("-", this);
        }
        return new IntegerNumber(-n);
    }

    @Override
    public IntegerNumber abs() throws OverflowException {
        if (n > 0) {
            return this;
        } else {
            return this.negate();
        }
    }

    public IntegerNumber sqrt() throws IllegalArgumentException {
        if (this.n < 0) {
            throw new IllegalArgumentException("sqrt", this);
        }

        return binarySearch(ONE.negate(), this.n == Integer.MAX_VALUE ? this : this.add(ONE), (n) -> {
            try {
                return n.multiply(n).greaterThan(this);
            } catch (OverflowException e) {
                return true;
            }
        }).subtract(ONE);
    }

    public IntegerNumber pow(IntegerNumber that) throws OverflowException {
        if (this.n == 0 && that.n == 0 || that.n < 0) {
            throw new IllegalArgumentException(this, "**", that);
        }

        IntegerNumber argument = this;
        IntegerNumber ans = ONE;

        while (that.greaterThan(ZERO)) {
            if (that.modulo(TWO).equalsTo(ZERO)) {
                argument = argument.multiply(argument);
                that = that.divide(TWO);
            } else {
                ans = ans.multiply(argument);
                that = that.subtract(ONE);
            }
        }

        return ans;
    }

    public IntegerNumber log(IntegerNumber that) {
        if (this.n <= 0 || that.n < 2) {
            throw new IllegalArgumentException(this, "//", that);
        }

        return binarySearch(ONE.negate(), new IntegerNumber(32), (n) -> {
            try {
                return that.pow(n).greaterThan(this);
            } catch (OverflowException e) {
                return true;
            }
        }).subtract(ONE);
    }

    @Override
    public int compareTo(IntegerNumber that) {
        return Integer.compare(this.n, that.n);
    }

    public static final IntegerNumber ZERO = new IntegerNumber(0);
    public static final IntegerNumber ONE = new IntegerNumber(1);
    public static final IntegerNumber TWO = new IntegerNumber(2);

    public int getN() {
        return n;
    }

    @Override
    public String toString() {
        return Integer.toString(n);
    }

    private static IntegerNumber binarySearch(IntegerNumber left, IntegerNumber right, Predicate<IntegerNumber> f) {
        while (left.add(ONE).lessThan(right)) {
            IntegerNumber mid = left.add(right.subtract(left).divide(TWO));
            if (f.test(mid)) {
                right = mid;
            } else {
                left = mid;
            }
        }

        return right;
    }

    @Override
    public IntegerNumber square() {
        return this.multiply(this);
    }
}
