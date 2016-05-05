/**
 * Created by artem on 02/03/15.
 */
public class ModuleSum {
    public static void main(String[] args) {
        ArrayQueueModule.clear();
        ArrayQueueModule.enqueue(0);

        for (String arg : args) {
            for (String number : arg.split("\\p{javaWhitespace}+")) {
                if (!number.isEmpty()) {
                    ArrayQueueModule.enqueue(Integer.parseInt(number));
                }
            }
        }

        while (ArrayQueueModule.size() > 1) {
            ArrayQueueModule.enqueue((int) ArrayQueueModule.dequeue() + (int) ArrayQueueModule.dequeue());
        }

        System.out.println(ArrayQueueModule.element());
    }
}
