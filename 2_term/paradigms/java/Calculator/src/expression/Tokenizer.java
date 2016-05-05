package expression;

import java.util.ArrayList;
import java.util.function.Predicate;

/**
 * Created by artem on 20/03/15.
 */
public class Tokenizer {
    /**
     * LEFT_BR      (
     * RIGHT_BR     )
     * PLUS         +
     * MINUS        -
     * TIMES        *
     * SLASH        /
     * NAME         [a-zA-Z]+
     * NUMBER       [0-9]+
     */
    public static enum TokenType {
        LEFT_BR, RIGHT_BR, PLUS, MINUS, TIMES, SLASH, NAME, NUMBER, LEFT_SHIFT, RIGHT_SHIFT
    }

    public static class Token {
        private TokenType type;
        private String string;

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

    private static String s;
    private static int i;

    public static ArrayList<Token> tokenize(String string) {
        s = string;
        i = 0;
        ArrayList<Token> tokens = new ArrayList<>();

        while (i < s.length()) {
            while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
                ++i;
            }
            if (i == s.length()) {
                break;
            }

            switch (s.charAt(i)) {
                case '(':
                    tokens.add(new Token(TokenType.LEFT_BR, "("));
                    ++i;
                    break;

                case ')':
                    tokens.add(new Token(TokenType.RIGHT_BR, ")"));
                    ++i;
                    break;

                case '+':
                    tokens.add(new Token(TokenType.PLUS, "+"));
                    ++i;
                    break;

                case '-':
                    tokens.add(new Token(TokenType.MINUS, "-"));
                    ++i;
                    break;

                case '*':
                    tokens.add(new Token(TokenType.TIMES, "*"));
                    ++i;
                    break;

                case '/':
                    tokens.add(new Token(TokenType.SLASH, "/"));
                    ++i;
                    break;

                case '<':
                    tokens.add(new Token(TokenType.LEFT_SHIFT, "<<"));
                    i += 2;
                    break;

                case '>':
                    tokens.add(new Token(TokenType.RIGHT_SHIFT, ">>"));
                    i += 2;
                    break;

                default:
                    String word;
                    if (Character.isAlphabetic(s.charAt(i))) {
                        word = takeWhile(Character::isAlphabetic);
                        tokens.add(new Token(TokenType.NAME, word));
                    } else if (Character.isDigit(s.charAt(i))) {
                        word = takeWhile(Character::isDigit);
                        tokens.add(new Token(TokenType.NUMBER, word));
                    }
                    break;
            }
        }

        s = null;
        return tokens;
    }

    private static String takeWhile(Predicate<Character> f) {
        StringBuilder word = new StringBuilder();
        for (; i < s.length() && f.test(s.charAt(i)); ++i) {
            word.append(s.charAt(i));
        }
        return word.toString();
    }
}
