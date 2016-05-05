/**
 * Created by artem on 13/02/15.
 */
public class Sum {
    public static void main(String[] args) {
        int result = 0;

        for (String arg : args) {
            for (String number : arg.split("[^-0-9]")) {
                if (!number.isEmpty()) {
                    result += Integer.parseInt(number);
                }
            }
        }

        System.out.println(result);
    }
}