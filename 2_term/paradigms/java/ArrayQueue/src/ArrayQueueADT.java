/**
 * Created by artem on 27/02/15.
 */
public class ArrayQueueADT {
    private final static int DEFAULT_ARRAY_SIZE = 8;

    private Object a[] = new Object[DEFAULT_ARRAY_SIZE];
    private int l = 0, r = 0, size = 0;

    private static void ensureCapacity(ArrayQueueADT queue) {
        if (queue.size == queue.a.length) {
            Object newA[] = new Object[queue.a.length * 2];

            System.arraycopy(queue.a, queue.l, newA, newA.length - (queue.a.length - queue.l),
                    queue.a.length - queue.l);
            System.arraycopy(queue.a, 0, newA, 0, queue.r);

            queue.l += queue.a.length;
            queue.a = newA;
        }
    }

    public static void enqueue(ArrayQueueADT queue, Object e) {
        ensureCapacity(queue);
        queue.a[queue.r++] = e;

        if (queue.r == queue.a.length) {
            queue.r = 0;
        }

        ++queue.size;
    }

    public static void push(ArrayQueueADT queue, Object e) {
        ensureCapacity(queue);

        --queue.l;
        if (queue.l == -1)
            queue.l = queue.a.length - 1;

        queue.a[queue.l] = e;

        ++queue.size;
    }

    public static Object element(ArrayQueueADT queue) {
        return queue.a[queue.l];
    }

    public static Object peek(ArrayQueueADT queue) {
        int rl = queue.r - 1;
        if (rl == -1)
            rl = queue.a.length - 1;

        return queue.a[rl];
    }

    public static Object dequeue(ArrayQueueADT queue) {
        Object next = element(queue);
        queue.a[queue.l++] = null;

        if (queue.l == queue.a.length) {
            queue.l = 0;
        }

        --queue.size;
        return next;
    }

    public static Object remove(ArrayQueueADT queue) {
        queue.r = queue.r - 1;
        if (queue.r == -1)
            queue.r = queue.a.length - 1;

        Object last = queue.a[queue.r];
        queue.a[queue.r] = null;

        --queue.size;

        return last;
    }

    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    public static void clear(ArrayQueueADT queue) {
        queue.a = new Object[DEFAULT_ARRAY_SIZE];
        queue.l = queue.r = queue.size = 0;
    }
}
