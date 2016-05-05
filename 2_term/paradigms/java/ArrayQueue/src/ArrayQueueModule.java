import java.io.PrintStream;

/**
 * Created by artem on 27/02/15.
 */
public class ArrayQueueModule {
    private final static int DEFAULT_ARRAY_SIZE = 8;

    private static Object a[] = new Object[DEFAULT_ARRAY_SIZE];
    private static int l = 0, r = 0, size = 0;

    public static void printQueue(PrintStream out) {
        for (int i = 0; i < size; ++i)
            out.print(a[(l + i) % a.length] + " ");
        out.println();
    }

    private static void ensureCapacity() {
        if (size == a.length) {
            Object newA[] = new Object[a.length * 2];

            System.arraycopy(a, l, newA, newA.length - (a.length - l), a.length - l);
            System.arraycopy(a, 0, newA, 0, r);

            l += a.length;
            a = newA;
        }
    }

    public static void enqueue(Object e) {
        ensureCapacity();

        a[r++] = e;

        if (r == a.length) {
            r = 0;
        }

        ++size;
    }

    public static void push(Object e) {
        ensureCapacity();

        --l;
        if (l == -1)
            l = a.length - 1;

        a[l] = e;

        ++size;
    }

    public static Object element() {
        return a[l];
    }

    public static Object peek() {
        int rl = r - 1;
        if (rl == -1)
            rl = a.length - 1;

        return a[rl];
    }

    public static Object dequeue() {
        Object next = element();
        a[l++] = null;

        if (l == a.length) {
            l = 0;
        }

        --size;
        return next;
    }

    public static Object remove() {
        r = r - 1;
        if (r == -1)
            r = a.length - 1;

        Object last = a[r];
        a[r] = null;

        --size;

        return last;
    }

    public static int size() {
        return size;
    }

    public static boolean isEmpty() {
        return size == 0;
    }

    public static void clear() {
        a = new Object[DEFAULT_ARRAY_SIZE];
        l = r = size = 0;
    }
}
