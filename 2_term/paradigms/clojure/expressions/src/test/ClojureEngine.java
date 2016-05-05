package test;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureEngine implements Engine {
    public static final IFn HASH_MAP = Clojure.var("clojure.core", "hash-map");
    public static final IFn EVAL = Clojure.var("clojure.core", "eval");

    private IFn parsed;
    private String expression;

    public ClojureEngine(final String script) {
        Clojure.var("clojure.core", "load-file").invoke(script);
    }

    private <T> Result<T> invoke(final IFn f, final Object arg, final Class<T> token, final String context) {
        final Object result;
        try {
            result = f.invoke(arg);
        } catch (final Throwable e) {
            throw new EngineException("No error expected in " + context, e);
        }
        if (result == null) {
            throw new EngineException(String.format("Expected %s, found null\n%s", token.getSimpleName(), context), null);
        }
        if (!token.isAssignableFrom(result.getClass())) {
            throw new EngineException(String.format("Expected %s, found %s (%s)\n%s", token.getSimpleName(), result, result.getClass().getSimpleName(), context), null);
        }
        return new Result<>(context, token.cast(result));
    }

    @Override
    public void parse(final String expression) {
        parsed = invoke(EVAL, Clojure.read(expression), IFn.class, expression).value;
        this.expression = expression;
    }

    @Override
    public Result<Number> evaluate(final double[] vars) {
        final Object map = HASH_MAP.invoke("x", vars[0], "y", vars[1], "z", vars[2]);
        final String context = String.format("(expr %s)\nwhere expr = %s", map, expression);
        return invoke(parsed, map, Number.class, context);
    }

    @Override
    public Result<String> parsedToString() {
        throw new UnsupportedOperationException("parsedToString");
    }
}
