import java.math.BigInteger;

/**
 * Created by artem on 17/02/15.
 */
public class SumLongHex {
    public static void main(String[] args) {
        long result = 0;

        for (String arg : args) {
            for (String number : arg.split("\\p{javaWhitespace}+")) {
                if (!number.isEmpty()) {
                    number = number.toUpperCase();
                    if (number.length() > 2 && number.charAt(0) == '0' && number.charAt(1) == 'X') {
                        result += Long.parseUnsignedLong(number.substring(2), 16);
                    }
                    else {
                        result += Long.parseLong(number);
                    }
                }
            }
        }

        System.out.println(result);
    }
}
