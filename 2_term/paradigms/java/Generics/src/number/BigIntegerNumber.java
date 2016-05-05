package number;

import expression.BinaryFunction;
import expression.parser.IllegalTokenException;

import java.math.BigInteger;
import java.util.function.Predicate;

public class BigIntegerNumber implements Number<BigIntegerNumber> {
    private final BigInteger n;

    public BigIntegerNumber(BigInteger n) {
        this.n = n;
    }

    public BigIntegerNumber(String str) throws IllegalTokenException {
        try {
            this.n = new BigInteger(str);
        } catch (NumberFormatException e) {
            throw new IllegalTokenException(str, e);
        }
    }

    public BigIntegerNumber(int n) {
        this.n = new BigInteger("" + n);
    }

    @Override
    public BigIntegerNumber add(BigIntegerNumber that) {
        return new BigIntegerNumber(n.add(that.n));
    }

    @Override
    public BigIntegerNumber subtract(BigIntegerNumber that) {
        return new BigIntegerNumber(n.subtract(that.n));
    }

    @Override
    public BigIntegerNumber multiply(BigIntegerNumber that) {
        return new BigIntegerNumber(n.multiply(that.n));
    }

    @Override
    public BigIntegerNumber divide(BigIntegerNumber that) {
        if (that.n.compareTo(BigInteger.ZERO) == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "/", that);
        }
        return new BigIntegerNumber(n.divide(that.n));
    }

    public BigIntegerNumber modulo(BigIntegerNumber that) {
        if (that.n.compareTo(BigInteger.ZERO) == 0) {
            //throw new DivisionByZeroException(this);
            throw new IllegalArgumentException(this, "/", that);
        }
        return new BigIntegerNumber(n.remainder(that.n));
    }

    @Override
    public BigIntegerNumber negate() {
        return new BigIntegerNumber(n.negate());
    }

    @Override
    public BigIntegerNumber abs() {
        return new BigIntegerNumber(n.abs());
    }

    public BigIntegerNumber sqrt() {
        if (this.lessThan(ZERO)) {
            throw new IllegalArgumentException("sqrt", this);
        }

        return binarySearch(ONE.negate(), this.add(ONE),
                (n) -> n.multiply(n).greaterThan(this)
        ).subtract(ONE);
    }

    public BigIntegerNumber pow(BigIntegerNumber that) {
        if (this.equalsTo(ZERO) && that.equalsTo(ZERO) || that.lessThan(ZERO)) {
            throw new IllegalArgumentException(this, "**", that);
        }

        BigIntegerNumber argument = this;
        BigIntegerNumber ans = ONE;

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

    public BigIntegerNumber log(BigIntegerNumber that) {
        if (!this.greaterThan(ZERO) || that.lessThan(TWO)) {
            throw new IllegalArgumentException(this, "//", that);
        }

        return binarySearch(ONE.negate(), this, (n) -> {
            try {
                return that.pow(n).greaterThan(this);
            } catch (OverflowException e) {
                return true;
            }
        }).subtract(ONE);
    }

    @Override
    public int compareTo(BigIntegerNumber that) {
        return n.compareTo(that.n);
    }

    public static BigIntegerNumber ZERO = new BigIntegerNumber(BigInteger.ZERO);
    public static BigIntegerNumber ONE = new BigIntegerNumber(BigInteger.ONE);
    public static BigIntegerNumber TWO = new BigIntegerNumber(new BigInteger("2"));

    public BigInteger getN() {
        return n;
    }

    @Override
    public String toString() {
        return n.toString();
    }

    private static BigIntegerNumber binarySearch(BigIntegerNumber left, BigIntegerNumber right,
                                                 Predicate<BigIntegerNumber> f) {
        while (left.add(ONE).lessThan(right)) {
            BigIntegerNumber mid = left.add(right.subtract(left).divide(TWO));
            if (f.test(mid)) {
                right = mid;
            } else {
                left = mid;
            }
        }

        return right;
    }

    @Override
    public BigIntegerNumber square() {
        return this.multiply(this);
    }
}
