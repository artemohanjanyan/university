import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Created by artem on 05/03/15.
 */
public abstract class AbstractQueue implements Queue {
    protected int size;

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    public abstract AbstractQueue getEmptyQueue();

    public Queue filter(Predicate<Object> p) {
        Queue queue = getEmptyQueue();

        Object array[] = toArray();
        for (Object e : array) {
            if (p.test(e)) {
                queue.enqueue(e);
            }
        }

        return this.getClass().cast(queue);
    }

    public Queue map(Function<Object, Object> f) {
        Queue queue = getEmptyQueue();

        Object array[] = toArray();
        for (Object e : array) {
            queue.enqueue(f.apply(e));
        }

        return this.getClass().cast(queue);
    }
}
