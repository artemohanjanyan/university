import java.net.MalformedURLException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class QueueToArrayTest extends ArrayQueueToArrayTest {
    public static void main(final String[] args) throws MalformedURLException, ClassNotFoundException, NoSuchMethodException {
        new QueueToArrayTest().test();
    }

    public void test() throws NoSuchMethodException, MalformedURLException, ClassNotFoundException {
        test("LinkedQueue", 2, Mode.CLASS);
        test("ArrayQueue", 2, Mode.CLASS);
    }
}
