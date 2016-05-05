package expression.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import static expression.parser.CheckedParser.*;

/**
 * Created by artem on 20/03/15.
 */
public class CheckedTokenizer {
    private static String exprString;
    private static int i;

    public static class TrieNode {
        public Token terminalOutput;
        public Map<Character, TrieNode> edges = new HashMap<>();
    }

    static TrieNode head;

    static {
        head = new TrieNode();
        for (Token token : CheckedParser.operatorList) {
            TrieNode v = head;
            for (Character c : token.getString().toCharArray()) {
                if (v.edges.get(c) == null) {
                    v.edges.put(c, new TrieNode());
                }
                v = v.edges.get(c);
            }
            v.terminalOutput = token;
        }
    }

    public ArrayList<Token> tokenize(String string) throws IllegalTokenException {
        TrieNode curNode = head;
        ArrayList<Token> tokens = new ArrayList<>();

        int i = 0;
        StringBuilder curStr = new StringBuilder();
        while (i < string.length()) {
            Character c = string.charAt(i++);

            if (curNode.edges.get(c) != null) {
                curNode = curNode.edges.get(c);
                curStr.append(c);
            } else {
                if (!curStr.toString().equals("") && curNode.terminalOutput != null) {
                    tokens.add(curNode.terminalOutput);
                    curStr = new StringBuilder();
                    curNode = head;
                    --i;
                    continue;
                }

                if (Character.isDigit(c) || Character.isAlphabetic(c)) {
                    Predicate<Character> predicate = Character.isDigit(c) ?
                            Character::isDigit :
                            Character::isAlphabetic;

                    boolean fl = true;
                    for (Character strC : curStr.toString().toCharArray()) {
                        fl &= predicate.test(strC);
                    }

                    if (!fl) {
                        throw new IllegalTokenException(curStr.toString() + c);
                    }

                    curStr.append(c);
                    while (i < string.length() && predicate.test(string.charAt(i))) {
                        curStr.append(string.charAt(i++));
                    }

                    tokens.add(new Token(Character.isDigit(c) ? TokenType.NUMBER : TokenType.NAME, curStr.toString()));
                    curStr = new StringBuilder();
                    curNode = head;
                } else if (curStr.toString().isEmpty() && Character.isWhitespace(c)) {
                    while (i < string.length() && Character.isWhitespace(string.charAt(i))) {
                        ++i;
                    }
                } else {
                    throw new IllegalTokenException(curStr.toString() + c);
                }
            }
        }

        if (curNode.terminalOutput == null) {
            throw new IllegalTokenException(curStr.toString());
        } else {
            tokens.add(curNode.terminalOutput);
        }
        tokens.add(head.terminalOutput);

        return tokens;
    }
}