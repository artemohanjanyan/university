package expression;

import java.util.ArrayList;

import static expression.Tokenizer.*;

/**
 * Created by artem on 19/03/15.
 * <p>
 * Parses input String according to following grammar:
 * <pre>
 *
 * ShiftExpression
 *     ShiftExpression << Expression
 *     ShiftExpression >> Expression
 *     Expression
 *
 * Expression:
 *     Expression + Term
 *     Expression - Term
 *     Term
 *
 * Term:
 *     Term * Primary
 *     Term / Primary
 *     Term mod Primary
 *     Primary
 *
 * Primary:
 *     NUMBER
 *     NAME
 *     -NUMBER
 *     -Primary
 *     abs Primary
 *     square Primary
 *     (Expression)
 * </pre>
 */
public class ExpressionParser implements Parser {
    private static ArrayList<Token> tokens;
    private static int i;

    @Override
    public TripleExpression parse(String s) {
        tokens = tokenize(s);
        i = 0;

        TripleExpression expression = getShiftExpression();
        tokens = null;

        return expression;
    }

    /**
     * <pre>
     * ShiftExpression
     *     ShiftExpression << Expression
     *     ShiftExpression >> Expression
     *     Expression
     */
    private static AbstractExpression getShiftExpression() {
        AbstractExpression acc = getExpression();

        while (i < tokens.size()) {
            Token operation = tokens.get(i++);

            switch (operation.getType()) {
                case LEFT_SHIFT:
                    acc = new LeftShift(acc, getExpression());
                    break;

                case RIGHT_SHIFT:
                    acc = new RightShift(acc, getExpression());
                    break;

                default:
                    --i;
                    return acc;
            }
        }

        return acc;
    }

    /**
     * <pre>
     * Expression:
     *     Expression + Term
     *     Expression - Term
     *     Term
     */
    private static AbstractExpression getExpression() {
        AbstractExpression acc = getTerm();

        while (i < tokens.size()) {
            Token operation = tokens.get(i++);

            switch (operation.getType()) {
                case PLUS:
                    acc = new Add(acc, getTerm());
                    break;

                case MINUS:
                    acc = new Subtract(acc, getTerm());
                    break;

                default:
                    --i;
                    return acc;
            }
        }

        return acc;
    }

    /**
     * <pre>
     * Term:
     *     Term * Primary
     *     Term / Primary
     *     Term mod Primary
     *     Primary
     */
    private static AbstractExpression getTerm() {
        AbstractExpression acc = getPrimary();

        while (i < tokens.size()) {
            Token operation = tokens.get(i++);

            switch (operation.getType()) {
                case TIMES:
                    acc = new Multiply(acc, getPrimary());
                    break;

                case SLASH:
                    acc = new Divide(acc, getPrimary());
                    break;

                case NAME: // mod
                    acc = new Mod(acc, getPrimary());
                    break;

                default:
                    --i;
                    return acc;
            }
        }

        return acc;
    }

    /**
     * <pre>
     *  Primary:
     *     NUMBER
     *     NAME
     *     -NUMBER
     *     -Primary
     *     abs Primary
     *     square Primary
     *     (ShiftExpression)
     */
    private static AbstractExpression getPrimary() {
        Token token = tokens.get(i++);
        AbstractExpression primary = null;

        switch (token.getType()) {
            case NUMBER:
                primary = new Const(Integer.parseInt(token.getString()));
                break;

            case NAME:
                switch (token.getString()) {
                    case "abs":
                        primary = new Abs(getPrimary());
                        break;

                    case "square":
                        AbstractExpression argument = getPrimary();
                        primary = new Square(argument);
                        break;

                    default:
                        primary = new Variable(token.getString());
                        break;
                }
                break;

            case MINUS:
                if (i < tokens.size() && tokens.get(i).getType() == TokenType.NUMBER) {
                    Token number = tokens.get(i++);
                    primary = new Const(Integer.parseInt(token.getString() + number.getString()));
                } else {
                    primary = new Multiply(new Const(-1), getPrimary());
                }
                break;

            case LEFT_BR:
                primary = getShiftExpression();
                ++i;
                break;
        }

        return primary;
    }
}
