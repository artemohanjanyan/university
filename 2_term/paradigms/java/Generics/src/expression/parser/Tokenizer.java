package expression.parser;

import number.Number;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static expression.parser.Parser.*;

class Tokenizer<T extends Number<T>> {
    class TrieNode {
        public Parser.Token<T> terminalOutput;
        public Map<Character, TrieNode> edges = new HashMap<>();
    }

    private TrieNode head;

    private Reader<T> constantReader;
    private VariableReader variableReader = new VariableReader();

    Tokenizer(Parser<T> parser, Reader<T> constantReader) {
        this.constantReader = constantReader;

        head = new TrieNode();
        for (Parser.Token<T> token : parser.operatorList) {
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

    ArrayList<Parser.Token<T>> tokenize(String string) throws IllegalTokenException {
        TrieNode curNode = head;
        ArrayList<Parser.Token<T>> tokens = new ArrayList<>();

        int i = 0, curLength = 0;
        while (i < string.length()) {
            Character c = string.charAt(i++);

            if (curNode.edges.get(c) != null) {
                curNode = curNode.edges.get(c);
                ++curLength;
            } else {
                if (curLength != 0 && curNode.terminalOutput != null) {
                    tokens.add(curNode.terminalOutput);
                    curLength = 0;
                    curNode = head;
                    --i;
                    continue;
                }

                if (constantReader.assume(c) || variableReader.assume(c)) {
                    Reader reader = constantReader.assume(c) ? constantReader : variableReader;
                    ++curLength;

                    int startI = i - curLength;
                    int endI = reader.skip(string, startI);
                    if (endI < i) {
                        throw new IllegalTokenException(string.substring(startI, i));
                    }

                    tokens.add(new Parser.Token<>(
                            reader.getType(),
                            string.substring(startI, endI)));

                    i = endI;
                    curNode = head;
                    curLength = 0;
                    continue;
                }

                if (curLength == 0 && Character.isWhitespace(c)) {
                    while (i < string.length() && Character.isWhitespace(string.charAt(i))) {
                        ++i;
                    }
                } else {
                    throw new IllegalTokenException(string.substring(i - curLength, i));
                }
            }
        }

        if (curNode.terminalOutput == null) {
            throw new IllegalTokenException(string.substring(i - curLength, i));
        } else {
            tokens.add(curNode.terminalOutput);
        }
        tokens.add(head.terminalOutput);

        return tokens;
    }

    private static class VariableReader implements Reader<String> {
        @Override
        public int skip(String s, int i) {
            while (i < s.length() && Character.isAlphabetic(s.charAt(i))) {
                ++i;
            }
            return i;
        }

        @Override
        public boolean assume(char c) {
            return Character.isAlphabetic(c);
        }

        @Override
        public String create(String s) throws IllegalTokenException {
            return s;
        }

        @Override
        public TokenType getType() {
            return TokenType.NAME;
        }
    }
}