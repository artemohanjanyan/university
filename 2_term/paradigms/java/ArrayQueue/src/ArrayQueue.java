/**
 * Created by artem on 27/02/15.
 */
public class ArrayQueue {
    private final static int DEFAULT_ARRAY_SIZE = 8;

    private Object a[] = new Object[DEFAULT_ARRAY_SIZE];
    private int l = 0, r = 0, size = 0;

    private void ensureCapacity() {
        if (size == a.length) {
            Object newA[] = new Object[a.length * 2];

            System.arraycopy(a, l, newA, newA.length - (a.length - l), a.length - l);
            System.arraycopy(a, 0, newA, 0, r);

            l += a.length;
            a = newA;
        }
    }

    // true
    // ----------------
    // a[r++] = e
    public void enqueue(Object e) {
        ensureCapacity();
        a[r++] = e;

        if (r == a.length) {
            r = 0;
        }

        ++size;
    }

    // true
    // ----------------
    // a[--l] = e
    public void push(Object e) {
        ensureCapacity();

        --l;
        if (l == -1)
            l = a.length - 1;

        a[l] = e;

        ++size;
    }

    // size() > 0
    // ----------------
    // result = a[l]
    public Object element() {
        return a[l];
    }

    // size() > 0
    // ----------------
    // result = a[r - 1]
    public Object peek() {
        int rl = r - 1;
        if (rl == -1) {
            rl = a.length - 1;
        }
        return a[rl];
    }

    // size() > 0
    // ----------------
    // result = a[l']
    // l = l' + 1
    public Object dequeue() {
        Object next = element();
        a[l++] = null;

        if (l == a.length) {
            l = 0;
        }

        --size;
        return next;
    }

    // size() > 0
    // ----------------
    // result = a[r' - 1]
    // r = r' - 1
    // a[r' - 1] == null
    public Object remove() {
        r = r - 1;
        if (r == -1) {
            r = a.length - 1;
        }

        Object last = a[r];
        a[r] = null;

        --size;

        return last;
    }

    // true
    // ----------------
    // result = r - l
    public int size() {
        return size;
    }

    // true
    // ----------------
    // result = size() == 0
    public boolean isEmpty() {
        return size == 0;
    }

    // true
    // ----------------
    // a == []
    // l == r == 0
    public void clear() {
        a = new Object[DEFAULT_ARRAY_SIZE];
        l = r = size = 0;
    }

    // true
    // ----------------
    // result[i] = a[l + i]
    // result.length = size()
    public Object[] toArray() {
        Object array[] = new Object[size];

        for (int i = 0; i < size; ++i)
            array[i] = a[(l + i) % a.length];

        return array;
    }
}
