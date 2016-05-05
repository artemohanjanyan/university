/**
 * Created by artem on 06/03/15.
 */

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * {@inheritDoc}
 * Elements are stored in a linked list.
 * Thus structure uses O(n) memory and complexity of operations on single elements is O(1).
 */
public class LinkedDeque extends AbstractDeque {
    private static class Node {
        Object value;
        Node next, previous;

        Node(Object value, Node next, Node previous) {
            this.value = value;
            this.next = next;
            this.previous = previous;
        }
    }

    private Node head, tail;

    public LinkedDeque() {
        head = tail = null;
        size = 0;
    }

    @Override
    public void enqueue(Object e) {
        if (tail == null) {
            tail = head = new Node(e, null, null);
        } else {
            tail = new Node(e, null, tail);
            tail.previous.next = tail;
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
        if (head != null) {
            head.previous = null;
        }

        --size;

        return dequeued;
    }

    @Override
    public void push(Object e) {
        if (head == null) {
            head = tail = new Node(e, null, null);
        } else {
            head = new Node(e, head, null);
            head.next.previous = head;
        }

        ++size;
    }

    @Override
    public Object remove() {
        Object removed = tail.value;

        if (head == tail) {
            head = null;
        }
        tail = tail.previous;
        if (tail != null) {
            tail.next = null;
        }

        --size;

        return removed;
    }

    @Override
    public Object element() {
        return head.value;
    }

    @Override
    public Object peek() {
        return tail.value;
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
            array[i++] = n;
        }

        return array;
    }

    @Override
    public LinkedDeque getEmptyQueue() {
        return new LinkedDeque();
    }
}
