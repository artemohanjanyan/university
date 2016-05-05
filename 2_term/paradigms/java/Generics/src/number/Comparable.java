package number;

public interface Comparable<T> extends java.lang.Comparable<T> {
    default boolean lessThan(T that) {
        return this.compareTo(that) < 0;
    }

    default boolean equalsTo(T that) {
        return this.compareTo(that) == 0;
    }

    default boolean greaterThan(T that) {
        return this.compareTo(that) > 0;
    }
}
