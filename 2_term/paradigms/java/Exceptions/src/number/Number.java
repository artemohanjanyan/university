package number;

import expression.parser.IllegalTokenException;

import java.util.function.Predicate;

/**
 * Created by Artem Ohanjanyan on 26/03/15.
 */

public class Number {
    private final int n;

    public Number(int n) {
        this.n = n;
    }

    public Number(String str) throws IllegalTokenException {
        try {
            this.n = Integer.parseInt(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    public Number add(Number that) throws OverflowException {
        if (that.n > 0 && Integer.MAX_VALUE - that.n < this.n ||
                that.n < 0 && Integer.MIN_VALUE - that.n > this.n) {
            throw new OverflowException(this, "+", that);
        }
        return new Number(this.n + that.n);
    }

    public Number subtract(Number that) throws OverflowException {
        if (that.n < 0 && Integer.MAX_VALUE + that.n < this.n ||
                that.n > 0 && Integer.MIN_VALUE + that.n > this.n) {
            throw new OverflowException(this, "-", that);
        }
        return new Number(this.n - that.n);
    }

    public Number multiply(Number that) throws OverflowException {
        if (this.n > 0 && that.n > 0 && Integer.MAX_VALUE / that.n < this.n ||
                this.n > 0 && that.n < 0 && Integer.MIN_VALUE / this.n > that.n ||
                this.n < 0 && that.n > 0 && Integer.MIN_VALUE / that.n > this.n ||
                this.n < 0 && that.n < 0 && Integer.MAX_VALUE / that.n > this.n) {
            throw new OverflowException(this, "*", that);
        }
        return new Number(this.n * that.n);
    }

    public Number divide(Number that) throws NumberOperationException {
        if (that.n == 0) {
            throw new DivisionByZeroException(this);
        }
        if (this.n == Integer.MIN_VALUE && that.n == -1) {
            throw new OverflowException(this, "/", that);
        }
        return new Number(this.n / that.n);
    }

    public Number modulo(Number that) throws DivisionByZeroException {
        if (that.n == 0) {
            throw new DivisionByZeroException(this);
        }
        return new Number(this.n % that.n);
    }

    public Number negate() throws OverflowException {
        if (n == Integer.MIN_VALUE) {
            throw new OverflowException("-", this);
        }
        return new Number(-n);
    }

    public Number abs() throws OverflowException {
        if (n > 0) {
            return this;
        } else {
            return this.negate();
        }
    }

    public Number square() throws OverflowException {
        return this.multiply(this);
    }

    public Number sqrt() throws IllegalArgumentException {
        if (this.n < 0) {
            throw new IllegalArgumentException("sqrt", this);
        }

        return binarySearch(ONE.negate(), this.n == Integer.MAX_VALUE ? this : this.add(ONE), (n) -> {
            try {
                return n.multiply(n).isGreater(this);
            } catch (OverflowException e) {
                return true;
            }
        }).subtract(ONE);
    }

    public Number pow(Number that) throws OverflowException {
        if (this.n == 0 && that.n == 0 || that.n < 0) {
            throw new IllegalArgumentException(this, " // ", that);
        }

        Number argument = this;
        Number ans = ONE;

        while (that.isGreater(ZERO)) {
            if (that.modulo(TWO).isEqual(ZERO)) {
                argument = argument.square();
                that = that.divide(TWO);
            } else {
                ans = ans.multiply(argument);
                that = that.subtract(ONE);
            }
        }

        return ans;
    }

    public Number log(Number that) {
        if (this.n <= 0 || that.n < 2) {
            throw new IllegalArgumentException("log", this);
        }

        return binarySearch(ONE.negate(), new Number(32), (n) -> {
            try {
                return that.pow(n).isGreater(this);
            } catch (OverflowException e) {
                return true;
            }
        }).subtract(ONE);
    }

    public Number leftShift(Number that) {
        return new Number(this.getN() << that.getN());
    }

    public Number rightShift(Number that) {
        return new Number(this.getN() >> that.getN());
    }

    public boolean isEqual(Number that) {
        return this.n == that.n;
    }

    public boolean isLess(Number that) {
        return this.n < that.n;
    }

    public boolean isGreater(Number that) {
        return this.n > that.n;
    }

    private static Number binarySearch(Number left, Number right, Predicate<Number> f) {
        while (left.add(ONE).isLess(right)) {
            Number mid = left.add(right.subtract(left).divide(TWO));
            if (f.test(mid)) {
                right = mid;
            } else {
                left = mid;
            }
        }

        return right;
    }

    public int getN() {
        return n;
    }

    public static final Number ZERO = new Number(0);
    public static final Number ONE = new Number(1);
    public static final Number TWO = new Number(2);

    @Override
    public String toString() {
        return Integer.toString(n);
    }
}
