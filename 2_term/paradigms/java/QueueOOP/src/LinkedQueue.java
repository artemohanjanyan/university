/**
 * Created by artem on 05/03/15.
 */

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * {@inheritDoc}
 * Elements are stored in a linked list.
 * Thus structure uses O(n) memory and complexity of operations on single elements is O(1).
 */
public class LinkedQueue extends AbstractQueue {
    private static class Node {
        Object value;
        Node next;

        Node(Object value, Node next) {
            this.value = value;
            this.next = next;
        }
    }

    private Node head, tail;

    public LinkedQueue() {
        head = tail = null;
        size = 0;
    }

    @Override
    public void enqueue(Object e) {
        if (tail == null) {
            tail = head = new Node(e, null);
        } else {
            Node newTail = new Node(e, null);
            tail.next = newTail;
            tail = newTail;
        }

        ++size;
    }

    @Override
    public Object dequeue() {
        Object dequeued = head.value;

        if (head == tail) {
            tail = null;
        }
        head = head.next;

        --size;

        return dequeued;
    }

    @Override
    public Object element() {
        return head.value;
    }

    @Override
    public void clear() {
        head = tail = null;
        size = 0;
    }

    @Override
    public Object[] toArray() {
        Object array[] = new Object[size];
        int i = 0;
        for (Node n = head; n != null; n = n.next) {
            array[i++] = n.value;
        }

        return array;
    }

    @Override
    public LinkedQueue getEmptyQueue() {
        return new LinkedQueue();
    }
}
