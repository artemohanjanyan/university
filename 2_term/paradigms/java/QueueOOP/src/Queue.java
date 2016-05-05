/**
 * Created by artem on 05/03/15.
 */

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * <pre>
 *        _ _ _ _ _ _     _ _ _ _ _
 *       |_|_|_|_|_|_ ... _|_|_|_|_|
 *       head                   tail
 *       element()
 *   <== dequeue()         enqueue() <==
 * </pre>
 */

// a[] - array, elements can be indexed by any arbitrary integer.
// l - index of the head of the queue
// r - index of the tail of the queue + 1
public interface Queue {
    /**
     * Appends the specified element to the tail of the queue.
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       element()
     *   <== dequeue()         <b>enqueue() <==</b>
     * </pre>
     *
     * @param e the element to add
     */
    // true
    // ----------------
    // a[r++] = e
    public void enqueue(Object e);

    /**
     * <p>
     * Retrieves and removes the head of the queue.
     * </p>
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       element()
     *   <b><== dequeue()</b>         enqueue() <==
     * </pre>
     *
     * <p>
     * <i>Pred:</i><bs>
     * The queue must be non-empty.
     * </p>
     *
     * @return the head of the queue
     */
    // size() > 0
    // ----------------
    // result = a[l']
    // l = l' + 1
    public Object dequeue();

    /**
     * Retrieves the head of the queue.
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       <b>element()</b>
     *   <== dequeue()         enqueue() <==
     * </pre>
     *
     * <p>
     * <i>Pred:</i><bs>
     * The queue must be non-empty.
     * </p>
     *
     * @return the head of the queue
     */
    // size() > 0
    // ----------------
    // result = a[l]
    public Object element();


    /**
     * Clears the queue.
     */
    // true
    // ----------------
    // a == []
    // l == r == 0
    public void clear();

    /**
     * @return the size of the queue
     */
    // true
    // ----------------
    // result = r - l
    public int size();

    /**
     * @return {@code true} if the queue contains no elements, {@code false} otherwise
     */
    // true
    // ----------------
    // result = size() == 0
    public boolean isEmpty();

    // true
    // ----------------
    // result[i] = a[l + i]
    // result.length = size()
    public Object[] toArray();

    public Queue filter(Predicate<Object> p);

    public Queue map(Function<Object, Object> f);
}
