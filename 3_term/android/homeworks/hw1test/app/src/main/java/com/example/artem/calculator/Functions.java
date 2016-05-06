package com.example.artem.calculator;

import java.math.BigDecimal;

public class Functions {
    public interface Function {
        BigDecimal execute(BigDecimal a, BigDecimal b);
    }

    public static class FlipConst implements Function {
        @Override
        public BigDecimal execute(BigDecimal a, BigDecimal b) {
            return b;
        }
    }

    public static class Add implements Function {
        @Override
        public BigDecimal execute(BigDecimal a, BigDecimal b) {
            return a.add(b);
        }
    }

    public static class Subtract implements Function {
        @Override
        public BigDecimal execute(BigDecimal a, BigDecimal b) {
            return a.subtract(b);
        }
    }

    public static class Multiply implements Function {
        @Override
        public BigDecimal execute(BigDecimal a, BigDecimal b) {
            return a.multiply(b);
        }
    }

    public static class Divide implements Function {
        @Override
        public BigDecimal execute(BigDecimal a, BigDecimal b) {
            return a.divide(b, 20, BigDecimal.ROUND_CEILING);
        }
    }
}
