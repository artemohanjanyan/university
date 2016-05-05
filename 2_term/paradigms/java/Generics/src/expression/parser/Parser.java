package expression.parser;

import expression.*;
import number.Number;

import java.rmi.MarshalledObject;
import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

import static expression.AbstractValue.*;
import static expression.UnaryFunction.*;
import static expression.BinaryFunction.*;

public class Parser<T extends Number<T>> {
    enum TokenType {
        LEFT_BR("opening parenthesis"),
        RIGHT_BR("closing parenthesis"),

        NAME("identifier"),
        NUMBER("number"),

        EOE("end of expression"),

        // Binary operators
        PLUS("plus sign"),
        MINUS("minus sign"),

        TIMES("times sign"),
        SLASH("division sign"),
        MOD("mod operation"),

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

    static class Token<T extends Number<T>> {
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

    static class UnaryOperator<T extends Number<T>> extends Token<T> {
        private Function<AbstractExpression<T>, AbstractExpression<T>> create;

        public UnaryOperator(TokenType type, String str,
                             Function<AbstractExpression<T>, AbstractExpression<T>> create) {
            super(type, str);
            this.create = create;
        }

        public AbstractExpression<T> create(AbstractExpression<T> arg) {
            return create.apply(arg);
        }
    }

    static class BinaryOperator<T extends Number<T>> extends Token<T> {
        private BiFunction<AbstractExpression<T>, AbstractExpression<T>, AbstractExpression<T>> create;
        private int priority;

        public BinaryOperator(TokenType type, String str,
                              BiFunction<AbstractExpression<T>, AbstractExpression<T>, AbstractExpression<T>> create,
                              int priority) {
            super(type, str);
            this.create = create;
            this.priority = priority;
        }

        public AbstractExpression<T> create(AbstractExpression<T> left, AbstractExpression<T> right) {
            return create.apply(left, right);
        }

        public int getPriority() {
            return priority;
        }
    }

    final ArrayList<Token<T>> operatorList;
    Reader<T> reader;
    Tokenizer<T> tokenizer;

    public Parser(Reader<T> reader) {
        operatorList = new ArrayList<>();
        operatorList.add(new Token<>(TokenType.LEFT_BR, "("));
        operatorList.add(new Token<>(TokenType.RIGHT_BR, ")"));
        operatorList.add(new Token<>(TokenType.EOE, ""));
        operatorList.add(new BinaryOperator<>(TokenType.PLUS, "+", Add::new, 2));
        operatorList.add(new BinaryOperator<>(TokenType.MINUS, "-", Subtract::new, 2));
        operatorList.add(new BinaryOperator<>(TokenType.TIMES, "*", Multiply::new, 1));
        operatorList.add(new BinaryOperator<>(TokenType.SLASH, "/", Divide::new, 1));
        operatorList.add(new BinaryOperator<>(TokenType.MOD, "mod", Mod::new, 1));
        //operatorList.add(new BinaryOperator<>(TokenType.POW, "**", Pow::new, 1));
        //operatorList.add(new BinaryOperator<>(TokenType.LOG, "//", Log::new, 1));
        operatorList.add(new UnaryOperator<>(TokenType.ABS, "abs", Abs::new));
        operatorList.add(new UnaryOperator<>(TokenType.SQUARE, "square", Square::new));
        //operatorList.add(new UnaryOperator<>(TokenType.SQRT, "sqrt", Sqrt::new));

        this.reader = reader;
        this.tokenizer = new Tokenizer<>(this, reader);
    }

    private ArrayList<Token<T>> tokens;
    private int i;

    public TripleExpression<T> parse(String s) throws ParserException {
        tokens = tokenizer.tokenize(s);
        i = 0;

        TripleExpression<T> expression = getExpression();
        if (tokens.get(i).getType() != TokenType.EOE) {
            throw new UnexpectedTokenException("binary operator", tokens.get(i).getType());
        }
        tokens = null;

        return expression;
    }

    private AbstractExpression<T> getExpression() throws ParserException {
        return getExpression(3);
    }

    private AbstractExpression<T> getExpression(int depth) throws ParserException {
        if (depth == 0) {
            return getPrimary();
        }

        AbstractExpression<T> acc = getExpression(depth - 1);

        while (tokens.get(i) instanceof BinaryOperator) {
            BinaryOperator<T> operator = (BinaryOperator<T>) tokens.get(i++);
            if (operator.getPriority() != depth) {
                --i;
                break;
            }

            AbstractExpression<T> right = getExpression(depth - 1);
            acc = operator.create(acc, right);
        }

        return acc;
    }

    private AbstractExpression<T> getPrimary() throws ParserException {
        Token<T> token = tokens.get(i++);
        AbstractExpression<T> primary;

        switch (token.getType()) {
            case NUMBER:
                primary = new Const<>(reader.create(token.getString()));
                break;

            case NAME:
                switch (token.getString()) {
                    case "x":
                    case "y":
                    case "z": {
                            primary = new Variable<>(token.getString());
                            break;
                    }

                    default:
                        throw new IllegalTokenException(token.getString());
                }
                break;

            case MINUS:
                if (i < tokens.size() && tokens.get(i).getType() == TokenType.NUMBER) {
                        Token number = tokens.get(i++);
                        primary = new Const<>(reader.create(token.getString() + number.getString()));
                } else {
                        primary = new Negate<>(getPrimary());
                }
                break;

            case LEFT_BR:
                primary = getExpression();
                skipToken(TokenType.RIGHT_BR);
                break;

            default: {
                if (token instanceof UnaryOperator) {
                    UnaryOperator<T> operator = (UnaryOperator<T>) token;
                    primary = operator.create(getPrimary());
                    break;
                }
                throw new UnexpectedTokenException("primary expression", token.getType());
            }
        }

        return primary;
    }

    private void skipToken(TokenType type) throws ParserException {
        if (tokens.get(i).getType() != type) {
            throw new UnexpectedTokenException(type, tokens.get(i).getType());
        }
        ++i;
    }
}
