/**
 * Created by artem on 21/02/15.
 */
public class BinarySearch {
    // for all i in [1..n): a[i - 1] >= a[i]
    public static int iterative_binary_search(int[] a, int x) {
        int l = -1, r = a.length;

        // (l == -1 || a[l] > x) && (r == a.length || a[r] <= x)
        while (l < r - 1) {
            assert (l < 0 || a[l] > x) && (r >= a.length || a[r] <= x);

            int mid = l + (r - l) / 2;
            if (a[mid] <= x) {
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


    // for all i in [1..n): a[i - 1] >= a[i]
    public static int recursive_binary_search(int[] a, int x) {
        return recursive_binary_search(a, x, -1, a.length);
    }
    // if (exists i: a[i] <= x) then
    //     result = min i: a[i] <= x
    // else
    //     result = a.length


    // for all i in [1..n): a[i - 1] >= a[i]
    // -1 <= l < r <= a.length
    // (l == -1 || a[l] > x) && (r == a.length || a[r] <= x)
    public static int recursive_binary_search(int[] a, int x, int l, int r) {
        if (l >= r - 1) {
            return r;
        }

        int mid = l + (r - l) / 2;
        if (a[mid] <= x) {
            r = mid;
        }
        else {
            l = mid;
        }

        return recursive_binary_search(a, x, l, r);
    }
    // if (exists i in ([l, r] intersect [0, a.length)): a[i] <= x) then
    //     result = min i: in ([l, r] intersect [0, a.length)): a[i] <= x
    // else
    //     result = r


    public static void main(String[] args) {
        int a[] = new int[args.length - 1];
        for (int i = 1; i < args.length; ++i) {
            a[i - 1] = Integer.parseInt(args[i]);
        }

        int x = Integer.parseInt(args[0]);

        System.out.println(iterative_binary_search(a, x));
    }
}
