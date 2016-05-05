package expression;

import number.Number;

import java.util.function.Function;

public abstract class UnaryFunction<T extends Number<T>> extends AbstractExpression<T> {
    private AbstractExpression<T> argument;
    private Function<T, T> function;

    public UnaryFunction(AbstractExpression<T> argument, Function<T, T> function) {
        this.argument = argument;
        this.function = function;
    }

    @Override
    public T evaluate(VariableContainer<T> variables) {
        return function.apply(argument.evaluate(variables));
    }

    @Override
    public String toString() {
        return super.toString() + "(" + argument.toString() + ")";
    }




    public static class Abs<T extends Number<T>> extends UnaryFunction<T> {
        public Abs(AbstractExpression<T> argument) {
            super(argument, T::abs);
        }
    }

    public static class Negate<T extends Number<T>> extends UnaryFunction<T> {
        public Negate(AbstractExpression<T> argument) {
            super(argument, T::negate);
        }
    }

    public static class Square<T extends Number<T>> extends UnaryFunction<T> {
        public Square(AbstractExpression<T> argument) {
            super(argument, T::square);
        }
    }

//    public static class Sqrt<T extends Number<T>> extends UnaryFunction<T> {
//        public Sqrt(AbstractExpression<T> argument) {
//            super(argument, T::sqrt);
//        }
//    }
}
