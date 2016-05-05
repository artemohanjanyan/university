/**
 * Created by artem on 21/02/15.
 */
public class BinarySearchMissing {
    interface Predicate {
        boolean test(int a);
    }

    // for all i in [1..n): a[i - 1] >= a[i]
    public static int iterative_binary_search(int[] a, Predicate predicate) {
        int l = -1, r = a.length;

        // (l == -1 || !predicate.test(a[l])) && (r == a.length || predicate.test(a[r]))
        while (l < r - 1) {
            assert (l == -1 || !predicate.test(a[l])) && (r == a.length || predicate.test(a[r]));

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
    // if (exists i: a[i] <= x) then
    //     result = min i: a[i] <= x
    // else
    //     result = a.length


    public static void main(String[] args) {
        int a[] = new int[args.length - 1];
        for (int i = 1; i < args.length; ++i) {
            a[i - 1] = Integer.parseInt(args[i]);
        }

        int x = Integer.parseInt(args[0]);

        int i1 = iterative_binary_search(a, (a1) -> a1 <= x);
        int length = iterative_binary_search(a, (a1) -> a1 < x) - 1 - i1;
        if (i1 == a.length || a[i1] != x)
            i1 = -i1 - 1;

        System.out.println(i1 + " " + length);
    }
}
