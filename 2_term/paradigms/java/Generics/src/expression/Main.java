package expression;

import expression.parser.Parser;
import expression.parser.ParserException;
import number.BigIntegerNumber;
import number.DoubleNumber;
import number.DoubleReader;
import number.NumberOperationException;

public class Main {
    public static void main(String[] args) throws ParserException {
        String s = "(1000000*x*x*x)*(x + 1)*x*x/(y-1)";
        int y = 3, z = 4;
        Parser<BigIntegerNumber> parser = new Parser<>(new number.BigIntegerReader());
        System.out.println("x\tf");
        for (int x = 0; x <= 10; ++x) {
            System.out.print(x + "\t");
            try {
                System.out.println(parser.parse(s).evaluate(
                        new BigIntegerNumber(x),
                        new BigIntegerNumber(y),
                        new BigIntegerNumber(z)));
            } catch (NumberOperationException e) {
                System.out.println(e.getClass().getSimpleName() + ": " + e.getMessage());
                //e.printStackTrace();
            }
        }

        StringBuilder builder = new StringBuilder();
        int n = 50001;
        for (int i = 0; i < n; ++i) {
            builder.append("-(");
        }
        builder.append(s);
        for (int i = 0; i < n; ++i) {
            builder.append(')');
        }
        TripleExpression<BigIntegerNumber> expression = parser.parse(builder.toString());
        System.out.println(expression.evaluate(
                new BigIntegerNumber(2),
                new BigIntegerNumber(3),
                new BigIntegerNumber(4)
        ));

        Parser<DoubleNumber> parser1 = new Parser<>(new DoubleReader());
        TripleExpression<DoubleNumber> expr = parser1.parse("abs (x - y) / z");
        System.out.println(expr);
    }
}
