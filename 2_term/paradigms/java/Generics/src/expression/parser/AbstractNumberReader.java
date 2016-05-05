package expression.parser;


public abstract class AbstractNumberReader<T extends number.Number> implements Reader<T> {
    @Override
    public Parser.TokenType getType() {
        return Parser.TokenType.NUMBER;
    }
}