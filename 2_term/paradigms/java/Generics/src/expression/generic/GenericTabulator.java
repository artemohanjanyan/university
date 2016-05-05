package expression.generic;

import expression.TripleExpression;
import expression.parser.AbstractNumberReader;
import expression.parser.Parser;
import number.*;
import number.Number;

import java.util.function.Function;

public class GenericTabulator implements Tabulator {
    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2)
            throws Exception {
        Tester tester;
        switch (mode) {
            case "i":
                tester = new Tester<>(IntegerNumber::new, IntegerNumber::getN, new IntegerReader());
                break;

            case "d":
                tester = new Tester<>(DoubleNumber::new, DoubleNumber::getX, new DoubleReader());
                break;

            case "bi":
                tester = new Tester<>(BigIntegerNumber::new, BigIntegerNumber::getN, new BigIntegerReader());
                break;

            case "u":
                tester = new Tester<>(UnsafeIntegerNumber::new, UnsafeIntegerNumber::getN, new UnsafeIntegerReader());
                break;

            case "b":
                tester = new Tester<>(UnsafeByteNumber::new, UnsafeByteNumber::getN, new UnsafeByteReader());
                break;

            case "f":
                tester = new Tester<>(FloatNumber::new, FloatNumber::getX, new FloatReader());
                break;

            default:
                throw new Exception("unknown mode " + mode);
        }

        return tester.test(expression, x1, x2, y1, y2, z1, z2);
    }

    private static class Tester<T extends Number<T>, R> {
        private Function<Integer, T> constructor;
        private Function<T, R> getter;
        private Parser<T> parser;

        public Tester(Function<Integer, T> constructor, Function<T, R> getter, AbstractNumberReader<T> reader) {
            this.constructor = constructor;
            this.getter = getter;
            this.parser = new Parser<>(reader);
        }

        public Object[][][] test(String expression, int x1, int x2, int y1, int y2, int z1, int z2)
                throws Exception {
            int xSize = x2 - x1 + 1;
            int ySize = y2 - y1 + 1;
            int zSize = z2 - z1 + 1;
            Object[][][] table = new Object[xSize][ySize][zSize];

            TripleExpression<T> tripleExpression = parser.parse(expression);

            for (int i = 0; i < xSize; ++i)  {
                for (int j = 0; j < ySize; ++j) {
                    for (int k = 0; k < zSize; ++k) {
                        try {
                            table[i][j][k] = getter.apply(tripleExpression.evaluate(
                                    constructor.apply(x1 + i),
                                    constructor.apply(y1 + j),
                                    constructor.apply(z1 + k)));
                        } catch (NumberOperationException e) {
                            table[i][j][k] = null;
                        }
                    }
                }
            }

            return table;
        }
    }
}
