/**
 * Created by artem on 02/03/15.
 */
public class ADTSum {
    public static void main(String[] args) {
        ArrayQueueADT queue = new ArrayQueueADT();

        ArrayQueueADT.clear(queue);
        ArrayQueueADT.enqueue(queue, 0);

        for (String arg : args) {
            for (String number : arg.split("\\p{javaWhitespace}+")) {
                if (!number.isEmpty()) {
                    ArrayQueueADT.enqueue(queue, Integer.parseInt(number));
                }
            }
        }

        while (ArrayQueueADT.size(queue) > 1) {
            ArrayQueueADT.enqueue(queue, (int) ArrayQueueADT.dequeue(queue) + (int) ArrayQueueADT.dequeue(queue));
        }

        System.out.println(ArrayQueueADT.element(queue));
    }
}
