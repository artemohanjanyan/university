package expression;

import expression.parser.CheckedParser;
import expression.parser.ParserException;
import number.DivisionByZeroException;
import number.OverflowException;

/**
 * Created by artem on 19/03/15.
 */
public class Main {
    public static void main(String[] args) throws ParserException {
        String s = "1000000*x*x*x*x*x/(x-1)";
        int y = 3, z = 4;
        CheckedParser parser = new CheckedParser();
        System.out.println("x\tf");
        for (int x = 0; x <= 10; ++x) {
            System.out.print(x + "\t");
            try {
                System.out.println(parser.parse(s).evaluate(x, y, z));
            } catch (OverflowException e) {
                System.out.println("overflow");
            } catch (DivisionByZeroException e) {
                System.out.println("division by zero");
            }
        }

        try {
            System.out.println(parser.parse("x // y").evaluate(1, 5, 0));
        } catch (ParserException e) {
            e.printStackTrace();
        }
    }
}
