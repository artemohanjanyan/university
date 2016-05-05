/**
 * Created by artem on 02/03/15.
 */
public class Sum {
    public static void main(String[] args) {
        ArrayQueue queue = new ArrayQueue();

        queue.clear();
        queue.enqueue(0);

        for (String arg : args) {
            for (String number : arg.split("\\p{javaWhitespace}+")) {
                if (!number.isEmpty()) {
                    queue.enqueue(Integer.parseInt(number));
                }
            }
        }

        while (queue.size() > 1) {
            queue.enqueue((int) queue.dequeue() + (int) queue.dequeue());
        }

        System.out.println(queue.element());
    }
}
