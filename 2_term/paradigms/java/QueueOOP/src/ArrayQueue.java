/**
 * Created by artem on 05/03/15.
 */

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * {@inheritDoc}
 * Elements are stored in a dynamic array.
 * Thus structure uses O(n) memory and complexity of operations on single elements is O(1) <b>amortized</b>.
 */
public class ArrayQueue extends AbstractQueue {
    private static final int DEFAULT_ARRAY_SIZE = 10;

    private int head, tail;
    private Object elements[];

    public ArrayQueue() {
        elements = new Object[DEFAULT_ARRAY_SIZE];
        tail = head = size = 0;
    }

    private void ensureCapacity() {
        if (size < elements.length) {
            return;
        }

        Object newElements[] = new Object[elements.length * 2];
        System.arraycopy(elements, head, newElements, head + elements.length, elements.length - head);
        System.arraycopy(elements, 0, newElements, 0, tail);
        head += elements.length;
        elements = newElements;
    }

    private int cycleIndex(int i) {
        return (i + elements.length) % elements.length;
    }

    @Override
    public void enqueue(Object e) {
        ensureCapacity();

        elements[tail] = e;
        tail = cycleIndex(tail + 1);
        ++size;
    }

    @Override
    public Object dequeue() {
        Object dequeued = elements[head];
        elements[head] = null;
        head = cycleIndex(head + 1);
        --size;
        return dequeued;
    }

    @Override
    public Object element() {
        return elements[head];
    }

    @Override
    public void clear() {
        elements = new Object[DEFAULT_ARRAY_SIZE];
        tail = head = size = 0;
    }

    @Override
    public Object[] toArray() {
        Object array[] = new Object[size];
        for (int i = 0; i < size; ++i) {
            array[i] = elements[(head + i) % elements.length];
        }

        return array;
    }

    @Override
    public ArrayQueue getEmptyQueue() {
        return new ArrayQueue();
    }
}
