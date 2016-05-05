import java.net.MalformedURLException;
import java.util.Arrays;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class QueueTest<T extends ArrayQueueTest.Queue> extends ArrayQueueTest<T> {
    public static void main(final String[] args) throws MalformedURLException, ClassNotFoundException, NoSuchMethodException {
        new QueueTest<>().test();
    }

    public void test() throws NoSuchMethodException, MalformedURLException, ClassNotFoundException {
        test("LinkedQueue", 2, Mode.CLASS);
        test("ArrayQueue", 2, Mode.CLASS);
    }

    @Override
    protected T create(final String className, final Mode mode) throws NoSuchMethodException, ClassNotFoundException, MalformedURLException {
        return check(super.create(className, mode));
    }

    protected T check(final T t) {
        final Class<?> clazz = t.instance.getClass();
        assertTrue(clazz + " should extend AbstractQueue", "AbstractQueue".equals(clazz.getSuperclass().getName()));
        assertTrue(clazz + " should implement interface Queue", implementsQueue(clazz) || implementsQueue(clazz.getSuperclass()));
        return t;
    }

    private boolean implementsQueue(final Class<?> clazz) {
        return Arrays.stream(clazz.getInterfaces()).anyMatch(iface -> "Queue".equals(iface.getName()));
    }

    private void assertTrue(final String message, final boolean equals) {
        if (!equals) {
            throw new AssertionError(message);
        }
    }
}
