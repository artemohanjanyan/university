package expression.parser;

import expression.*;
import number.Number;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

import static expression.AbstractValue.*;
import static expression.UnaryFunction.*;
import static expression.BinaryFunction.*;

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
 *     Term * Elementary
 *     Term / Elementary
 *     Term mod Elementary
 *     Elementary
 *
 * Elementary:
 *     Elementary ** Primary
 *     Elementary // Primary
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
public class CheckedParser implements Parser {
    enum TokenType {
        LEFT_BR("opening parenthesis"),
        RIGHT_BR("closing parenthesis"),

        NAME("identifier"),
        NUMBER("number"),

        EOE("end of expression"),

        // Binary operators
        //LEFT_SHIFT("left shift sign"),
        //RIGHT_SHIFT("right shift sign"),

        PLUS("plus sign"),
        MINUS("minus sign"),

        TIMES("times sign"),
        SLASH("division sign"),
        //MODULO("modulo operator"),

        POW("power sign"),
        LOG("logarithm sign"),

        // Unary operators
        ABS("abs"),
        SQUARE("square"),
        SQRT("square root");

        String string;

        TokenType(String string) {
            this.string = string;
        }

        @Override
        public String toString() {
            return string;
        }
    }

    static class Token {
        protected TokenType type;
        protected String string;

        public Token(TokenType type, String string) {
            this.type = type;
            this.string = string;
        }

        public TokenType getType() {
            return type;
        }

        public String getString() {
            return string;
        }
    }

    static class UnaryOperator extends Token {
        private Function<AbstractExpression, AbstractExpression> create;

        public UnaryOperator(TokenType type, String str,
                             Function<AbstractExpression, AbstractExpression> create) {
            super(type, str);
            this.create = create;
        }

        public AbstractExpression create(AbstractExpression arg) {
            return create.apply(arg);
        }
    }

    static class BinaryOperator extends Token {
        private BiFunction<AbstractExpression, AbstractExpression, AbstractExpression> create;
        private int priority;

        public BinaryOperator(TokenType type, String str,
                              BiFunction<AbstractExpression, AbstractExpression, AbstractExpression> create,
                              int priority) {
            super(type, str);
            this.create = create;
            this.priority = priority;
        }

        public AbstractExpression create(AbstractExpression left, AbstractExpression right) {
            return create.apply(left, right);
        }

        public int getPriority() {
            return priority;
        }
    }

    static final ArrayList<Token> operatorList;

    static {
        operatorList = new ArrayList<>();
        operatorList.add(new Token(TokenType.LEFT_BR, "("));
        operatorList.add(new Token(TokenType.RIGHT_BR, ")"));
        operatorList.add(new Token(TokenType.EOE, ""));

        operatorList.add(new BinaryOperator(TokenType.PLUS, "+", CheckedAdd::new, 3));
        operatorList.add(new BinaryOperator(TokenType.MINUS, "-", CheckedSubtract::new, 3));

        operatorList.add(new BinaryOperator(TokenType.TIMES, "*", CheckedMultiply::new, 2));
        operatorList.add(new BinaryOperator(TokenType.SLASH, "/", CheckedDivide::new, 2));

        operatorList.add(new BinaryOperator(TokenType.POW, "**", CheckedPow::new, 1));
        operatorList.add(new BinaryOperator(TokenType.LOG, "//", CheckedLog::new, 1));

        operatorList.add(new UnaryOperator(TokenType.ABS, "abs", CheckedAbs::new));
        operatorList.add(new UnaryOperator(TokenType.SQUARE, "square", CheckedSquare::new));
        operatorList.add(new UnaryOperator(TokenType.SQRT, "sqrt", CheckedSqrt::new));
    }

    private static ArrayList<Token> tokens;
    private static int i;

    @Override
    public TripleExpression parse(String s) throws ParserException {
        CheckedTokenizer tokenizer = new CheckedTokenizer();
        tokens = tokenizer.tokenize(s);
        i = 0;

        TripleExpression expression = getExpression();
        if (tokens.get(i).getType() != TokenType.EOE) {
            throw new UnexpectedTokenException("binary operator", tokens.get(i).getType());
        }
        tokens = null;

        return expression;
    }

    private static AbstractExpression getExpression() throws ParserException {
        return getExpression(3);
    }

    private static AbstractExpression getExpression(int depth) throws ParserException {
        if (depth == 0) {
            return getPrimary();
        }

        AbstractExpression acc = getExpression(depth - 1);

        while (tokens.get(i) instanceof BinaryOperator) {
            BinaryOperator operator = (BinaryOperator) tokens.get(i++);
            if (operator.getPriority() != depth) {
                --i;
                break;
            }

            AbstractExpression right = getExpression(depth - 1);
            acc = operator.create(acc, right);
        }

        return acc;
    }

    private static AbstractExpression getPrimary() throws ParserException {
        Token token = tokens.get(i++);
        AbstractExpression primary;

        switch (token.getType()) {
            case NUMBER:
                primary = new Const(new Number(token.getString()));
                break;

            case NAME:
                switch (token.getString()) {
                    case "x":
                    case "y":
                    case "z": {
                            primary = new Variable(token.getString());
                            break;
                    }

                    default:
                        throw new IllegalTokenException(token.getString());
                }
                break;

            case MINUS:
                if (i < tokens.size() && tokens.get(i).getType() == TokenType.NUMBER) {
                        Token number = tokens.get(i++);
                        primary = new Const(new Number(token.getString() + number.getString()));
                } else {
                        primary = new CheckedNegate(getPrimary());
                }
                break;

            case LEFT_BR:
                primary = getExpression();
                skipToken(TokenType.RIGHT_BR);
                break;

            default: {
                if (token instanceof UnaryOperator) {
                    UnaryOperator operator = (UnaryOperator) token;
                    primary = operator.create(getPrimary());
                    break;
                }
                throw new UnexpectedTokenException("primary expression", token.getType());
            }
        }

        return primary;
    }

    private static void skipToken(TokenType type) throws ParserException {
        if (tokens.get(i).getType() != type) {
            throw new UnexpectedTokenException(type, tokens.get(i).getType());
        }
        ++i;
    }
}
