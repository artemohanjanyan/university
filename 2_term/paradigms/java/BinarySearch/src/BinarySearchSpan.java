import java.util.function.Predicate;

/**
 * Created by artem on 21/02/15.
 */

public class BinarySearchSpan {
    // exists i:
    //      (forall j in (0..i) predicate.test(a[i]))
    //      && (forall j in [i, a.length): predicate.test(a[j]))
    public static int iterativeBinarySearch(int[] a, Predicate<Integer> predicate) {
        int l = -1, r = a.length;

        // (l == -1 || !predicate.test(a[l])) && (r == a.length || predicate.test(a[r]))
        while (l < r - 1) {
            int mid = l + (r - l) / 2;
            if (predicate.test(a[mid])) {
                r = mid;
            }
            else {
                l = mid;
            }
        }

        return r;
    }
    // if (exists i: predicate.test(a[i])) then
    //     result = min i: predicate.test(a[i])
    // else
    //     result = a.length


    public static void main(String[] args) {
        int a[] = new int[args.length - 1];
        for (int i = 1; i < args.length; ++i) {
            a[i - 1] = Integer.parseInt(args[i]);
        }

        int x = Integer.parseInt(args[0]);

        int i1 = iterativeBinarySearch(a, (a1) -> a1 <= x);
        int length = iterativeBinarySearch(a, (a1) -> a1 < x) - i1;

        System.out.println(i1 + " " + length);
    }
}

