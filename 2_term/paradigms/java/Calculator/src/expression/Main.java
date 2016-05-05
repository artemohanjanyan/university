package expression;

/**
 * Created by artem on 19/03/15.
 */
public class Main {
    public static void main(String[] args) {
        Parser expressionParser = new ExpressionParser();
        System.out.println(expressionParser.parse("-3 + abs(3    * x)"));
        System.out.println(expressionParser.parse("-3 + abs(3    * x)").evaluate(-2, 0, 0));
    }
}
