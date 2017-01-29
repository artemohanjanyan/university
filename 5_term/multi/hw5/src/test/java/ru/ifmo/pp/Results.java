package ru.ifmo.pp;

import java.util.Arrays;

/**
 * Results of operations on a bank.
 *
 * @author Roman Elizarov
 */
class Results {
    private final Object[] results;
    private int size;
    private int count;

    Results(int capacity) {
        results = new Object[capacity];
    }

    Results(Results other) {
        size = other.size;
        results = Arrays.copyOf(other.results, size);
    }

    void setSize(int size) {
        this.size = size;
    }

    Object get(int i) {
        return results[i];
    }

    void set(int i, Object val) {
        results[i] = val;
    }

    void incCount() {
        count++;
    }

    int getCount() {
        return count;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Results r = (Results) o;
        if (r.size != size)
            return false;
        for (int i = 0; i < size; i++) {
            Object o1 = results[i];
            Object o2 = r.results[i];
            if (!(o1==null ? o2==null : o1.equals(o2)))
                return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int result = 1;
        for (int i = 0; i < size; i++) {
            Object element = results[i];
            result = 31 * result + (element == null ? 0 : element.hashCode());
        }
        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        for (int i = 0; i < size; i++) {
            if (i > 0)
                sb.append(", ");
            sb.append(results[i]);
        }
        return sb.append(']').toString();
    }
}
