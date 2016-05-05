/**
 * Created by artem on 06/03/15.
 */

/**
 * <pre>
 *        _ _ _ _ _ _     _ _ _ _ _
 *       |_|_|_|_|_|_ ... _|_|_|_|_|
 *       head                   tail
 *       element()            peek()
 *   <== dequeue()         enqueue() <==
 *   ==> push()             remove() ==>
 * </pre>
 */

public interface Deque extends Queue {
    /**
     * Appends the specified element to the head of the queue.
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       element()            peek()
     *   <== dequeue()         enqueue() <==
     *   <b>==> push()</b>             remove() ==>
     * </pre>
     *
     * @param e the element to add
     */
    public void push(Object e);

    /**
     * <p>
     * Retrieves and removes the tail of the queue.
     * </p>
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       element()            peek()
     *   <== dequeue()         enqueue() <==
     *   ==> push()             <b>remove() ==></b>
     * </pre>
     *
     * <p>
     * <i>Pred:</i><bs>
     * The queue must be non-empty.
     * </p>
     *
     * @return the tail of the queue
     */
    public Object remove();

    /**
     * Retrieves the tail of the queue.
     *
     * <pre>
     *        _ _ _ _ _ _     _ _ _ _ _
     *       |_|_|_|_|_|_ ... _|_|_|_|_|
     *       head                   tail
     *       element()            <b>peek()</b>
     *   <== dequeue()         enqueue() <==
     *   ==> push()             remove() ==>
     * </pre>
     *
     * <p>
     * <i>Pred:</i><bs>
     * The queue must be non-empty.
     * </p>
     *
     * @return tail of the queue
     */
    public Object peek();
}
