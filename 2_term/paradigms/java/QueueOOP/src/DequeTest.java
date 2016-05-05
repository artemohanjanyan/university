import java.net.MalformedURLException;
import java.util.Deque;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class DequeTest extends QueueTestMy<DequeTest.Deque> {
    public static void main(final String[] args) throws MalformedURLException, ClassNotFoundException, NoSuchMethodException {
        new DequeTest().test();
    }

    @Override
    protected Deque create(final Mode mode) throws NoSuchMethodException, ClassNotFoundException, MalformedURLException {
        return new Deque(mode);
    }

    @Override
    protected void add(final java.util.Deque<Object> deque, final Deque queue, final Object element) {
        if (random.nextBoolean()) {
            super.add(deque, queue, element);
        } else {
            deque.addFirst(element);
            queue.push(element);
        }
    }

    @Override
    protected void check(final java.util.Deque<Object> deque, final Deque queue) {
        if (random.nextBoolean()) {
            super.check(deque, queue);
        } else {
            assertEquals("peek()", deque.getLast(), queue.peek());
        }
    }

    @Override
    protected void remove(final java.util.Deque<Object> deque, final Deque queue) {
        if (random.nextBoolean()) {
            super.remove(deque, queue);
        } else {
            assertEquals("remove()", deque.removeLast(), queue.remove());
        }
    }

    static class Deque extends QueueTestMy.Queue {
        private final ZMethod<Void> push;
        private final ZMethod<Object> peek;
        private final ZMethod<Object> remove;

        public Deque(final Mode mode) throws MalformedURLException, NoSuchMethodException, ClassNotFoundException {
            super("Deque", mode);
            push = findMethod("push", Object.class);
            peek = findMethod("peek");
            remove = findMethod("remove");
        }

        public void push(final Object element) { push.invoke(element); }
        public Object peek() { return peek.invoke(); }
        public Object remove() { return remove.invoke(); }
    }
}