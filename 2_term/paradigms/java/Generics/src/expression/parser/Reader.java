package expression.parser;

interface Reader<T> {
    int skip(String s, int i);

    boolean assume(char c);

    T create(String s) throws IllegalTokenException;

    Parser.TokenType getType();
}
