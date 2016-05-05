package number;

public interface Number<T extends Number<T>> extends Comparable<T> {
    T add(T that);

    T subtract(T that);

    T multiply(T that);

    T divide(T that);

    T modulo(T that);

    T negate();

    T abs();

    //T sqrt();

    T square();

//    T pow(T that);
//
//    T log(T that);
}
