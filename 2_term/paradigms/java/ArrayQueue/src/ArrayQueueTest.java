import java.io.File;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ArrayQueueTest<T extends ArrayQueueTest.Queue> {
    private static final Object[] ELEMENTS = new Object[]{
            "Hello",
            "world",
            1, 2, 3,
            new Object()
    };

    protected final Random random = new Random(2474258720358724587L);

    public static void main(final String[] args) throws MalformedURLException, ClassNotFoundException, NoSuchMethodException {
        new ArrayQueueTest().test();
    }

    protected void test() throws NoSuchMethodException, ClassNotFoundException, MalformedURLException {
        checkAssert();

        for (final Mode mode : Mode.values()) {
            System.out.println("Running " + getClass().getName() + " in " + mode + " mode");
            test(mode);
        }
        System.out.println("Done " + getClass().getName());
    }

    enum Mode {
        MODULE("Module") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                return checkStatic(true, new ZMethod<T>(instance, name, args));
            }
        },
        ADT("ADT") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                final Object[] a = new Object[args.length + 1];
                a[0] = instance;
                final Class<?>[] result = new Class<?>[args.length + 1];
                result[0] = instance.getClass();
                System.arraycopy(args, 0, result, 1, args.length);

                return checkStatic(true, new ZMethod<T>(instance, name, result) {
                    @Override
                    protected T invoke(final Object... args) {
                        if (args.length == 1) {
                            a[1] = args[0];
                        }
                        return super.invoke(a);
                    }
                });
            }
        },
        CLASS("") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                return checkStatic(false, new ZMethod<T>(instance, name, args));
            }
        };

        private final String suffix;

        Mode(final String suffix) {
            this.suffix = suffix;
        }

        private static <T> ZMethod<T> checkStatic(final boolean isStatic, final ZMethod<T> method) {
            if (isStatic != Modifier.isStatic(method.method.getModifiers())) {
                throw new AssertionError("Method " + method.method.getName() + " in " + method.method.getDeclaringClass() + (isStatic ? " must" : " must not") + " be static");
            }
            return method;
        }

        public abstract <T> ZMethod<T> getMethod(Object instance, String name, Class<?>... args) throws NoSuchMethodException;
    }

    protected static class ZMethod<T> {
        private final Object instance;
        private final Method method;

        public ZMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
            this.instance = instance;
            method = instance.getClass().getMethod(name, args);
        }

        protected T invoke(final Object... args) {
            try {
                @SuppressWarnings("unchecked")
                final T result = (T) method.invoke(instance, args);
                return result;
            } catch (final Exception e) {
                throw new AssertionError("Error calling method " + method.getName(), e);
            }
        }
    }

    protected static void checkAssert() {
        try {
            assert false;
            System.err.println("Assertions should be enabled");
            System.exit(1);
        } catch (final AssertionError e) {
            // ok
        }
    }

    private void test(final Mode mode) throws NoSuchMethodException, MalformedURLException, ClassNotFoundException {
        testEmpty(create(mode));
        testSingleton(create(mode));
        testClear(create(mode));
        for (int i = 0; i <= 10; i++) {
            testRandom(create(mode), 1_000_000, (double) i / 10);
        }
    }

    @SuppressWarnings("unchecked")
    protected T create(final Mode mode) throws NoSuchMethodException, ClassNotFoundException, MalformedURLException {
        return (T) new Queue("ArrayQueue", mode);
    }

    protected void testEmpty(final T queue) {
        System.out.println("=== testEmpty");
        assertSize(0, queue);
    }

    protected void testSingleton(final T queue) {
        System.out.println("=== testSingleton");
        assertSize(0, queue);
        final String value = "value";
        queue.enqueue(value);
        assertSize(1, queue);
        assertEquals("element()", value, queue.element());
        assertEquals("dequeue()", value, queue.dequeue());
        assertSize(0, queue);
    }

    protected void testClear(final T queue) {
        System.out.println("=== testClear");
        assertSize(0, queue);
        final String value = "value";
        queue.enqueue(value);
        queue.enqueue(value);
        queue.clear();
        assertSize(0, queue);
        final String value1 = "value1";
        queue.enqueue(value1);
        assertEquals("deque()", value1, queue.dequeue());
    }

    protected  void testRandom(final T queue, final int operations, final double addFreq) {
        System.out.println("=== testRandom, add frequency = " + addFreq);
        final Deque<Object> deque = new ArrayDeque<>();
        int ops = 0;
        for (int i = 0; i < operations; i++) {
            if (deque.isEmpty() || random.nextDouble() < addFreq) {
                add(deque, queue, randomElement());
            } else {
                remove(deque, queue);
            }
            checkAndSize(deque, queue);
            if (ops++ >= deque.size() && random.nextDouble() < 0.25) {
                ops -= deque.size();
                linearTest(deque, queue);
            }
        }
        linearTest(deque, queue);
        while (!deque.isEmpty()) {
            remove(deque, queue);
            checkAndSize(deque, queue);
        }
    }

    private void checkAndSize(final Deque<Object> deque, final T queue) {
        if (!deque.isEmpty() && random.nextBoolean()) {
            check(deque, queue);
        }
        assertSize(deque.size(), queue);
    }

    protected void remove(final Deque<Object> deque, final T queue) {
        assertEquals("dequeue()", deque.removeFirst(), queue.dequeue());
    }

    protected void check(final Deque<Object> deque, final T queue) {
        assertEquals("element()", deque.element(), queue.element());
    }

    protected void add(final Deque<Object> deque, final T queue, final Object element) {
        deque.addLast(element);
        queue.enqueue(element);
    }

    protected void linearTest(final Deque<Object> deque, final T queue) {
        // Do nothing by default
    }

    protected Object randomElement() {
        return ELEMENTS[random.nextInt(ELEMENTS.length)];
    }

    protected void assertSize(final int size, final T queue) {
        assertEquals("size()", size, queue.size());
        assert queue.size() == size : "Expected size() " + size + ", found " + queue.size();
        assert (size == 0) == queue.isEmpty() : "Expected isEmpty() " + (size == 0) + ", found " + queue.isEmpty();
    }

    protected static void assertEquals(final String message, final Object expected, final Object found) {
        assert expected.equals(found) : String.format("%s: expected %s, found %s", message, expected, found);
    }

    static class Queue {
        private final ZMethod<Void> enqueue;
        private final ZMethod<Object> element;
        private final ZMethod<Object> dequeue;
        private final ZMethod<Integer> size;
        private final ZMethod<Boolean> isEmpty;
        private final ZMethod<Void> clear;
        private final Class<?> clazz;
        private final Mode mode;
        private final Object instance;

        public Queue(final String name, final Mode mode) throws MalformedURLException, NoSuchMethodException, ClassNotFoundException {
            this.mode = mode;
            final String className = name + mode.suffix;
            final URL url = new File(".").toURI().toURL();
            clazz = new URLClassLoader(new URL[]{url}).loadClass(className);
            try {
                instance = clazz.newInstance();
            } catch (final Exception e) {
                throw new AssertionError("Cannot create instance of " + className, e);
            }
            enqueue = findMethod("enqueue", Object.class);
            element = findMethod("element");
            dequeue = findMethod("dequeue");
            size = findMethod("size");
            isEmpty = findMethod("isEmpty");
            clear = findMethod("clear");
        }

        protected <T> ZMethod<T> findMethod(final String name, final Class<?>... args) throws NoSuchMethodException {
            return mode.getMethod(instance, name, args);
        }

        public void enqueue(final Object element) { enqueue.invoke(element); }
        public Object element() { return element.invoke(); }
        public Object dequeue() { return dequeue.invoke(); }
        public int size() { return size.invoke(); }
        public boolean isEmpty() { return isEmpty.invoke(); }
        public void clear() { clear.invoke(); }
    }
}