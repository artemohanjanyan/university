package expression;

import number.Number;

import java.util.function.BiFunction;

public abstract class BinaryFunction<T extends Number<T>> extends AbstractExpression<T> {
    private BiFunction<T, T, T> function;

    private AbstractExpression<T> left, right;

    public BinaryFunction(AbstractExpression<T> left, AbstractExpression<T> right,
                          BiFunction<T, T, T> function) {
        this.left = left;
        this.right = right;
        this.function = function;
    }

    @Override
    public T evaluate(VariableContainer<T> variables) {
        return function.apply(left.evaluate(variables), right.evaluate(variables));
    }

    @Override
    public String toString() {
        return super.toString() + "(" + left.toString() + ", " + right.toString() + ")";
    }




    public static class Add<T extends Number<T>> extends BinaryFunction<T> {
        public Add(AbstractExpression<T> a, AbstractExpression<T> b) {
            super(a, b, T::add);
        }
    }

    public static class Subtract<T extends Number<T>> extends BinaryFunction<T> {
        public Subtract(AbstractExpression<T> a, AbstractExpression<T> b) {
            super(a, b, T::subtract);
        }
    }

    public static class Multiply<T extends Number<T>> extends BinaryFunction<T> {
        public Multiply(AbstractExpression<T> a, AbstractExpression<T> b) {
            super(a, b, T::multiply);
        }
    }

    public static class Divide<T extends Number<T>> extends BinaryFunction<T> {
        public Divide(AbstractExpression<T> a, AbstractExpression<T> b) {
            super(a, b, T::divide);
        }
    }

    public static class Mod<T extends Number<T>> extends BinaryFunction<T> {
        public Mod(AbstractExpression<T> a, AbstractExpression<T> b) {
            super(a, b, T::modulo);
        }
    }
//    public static class Log<T extends Number<T>> extends BinaryFunction<T> {
//        public Log(AbstractExpression<T> left, AbstractExpression<T> right) {
//            super(left, right, T::log);
//        }
//    }
//
//    public static class Pow<T extends Number<T>> extends BinaryFunction<T> {
//        public Pow(AbstractExpression<T> left, AbstractExpression<T> right) {
//            super(left, right, T::pow);
//        }
//    }
}
