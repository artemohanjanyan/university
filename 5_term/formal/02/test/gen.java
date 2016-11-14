import java.util.*;

public class gen {
    private static final int MAX_SIZE = 10;
    private static final int ALPH = 5;

    public static void main(String[] args) {
        final Random random;
        if (args.length == 0) {
            random = new Random();
        } else {
            random = new Random(Long.parseLong(args[0]));
        } 
        int n = random.nextInt(MAX_SIZE - 2) + 3;
        int m = random.nextInt(n * 3) + 1;
        int k_t = random.nextInt(n) + 1;
        int k = 0;
        boolean isTerm[] = new boolean[n];
        for (int i = 0; i < k_t; ++i) {
            int t = random.nextInt(n);
            if (!isTerm[t]) {
                k++;
                isTerm[t] = true;
            }
        }
        ArrayList<HashMap<Character, Integer>> dfa = new ArrayList<>();
        for (int i = 0; i < n; ++i) {
            dfa.add(new HashMap<>());
        }
        for (int i = 0; i < m; i++) {
            char chr = (char) (random.nextInt(ALPH) + 'a');
            int from = random.nextInt(n);
            int to = random.nextInt(n);
            dfa.get(from).put(chr, to);
        }

        m = dfa.stream().map(HashMap::size).reduce(0, (a, b) -> a + b);

        System.out.println(n + " " + m + " " + k);
        for (int i = 0; i < n; ++i) {
            if (isTerm[i]) {
                System.out.print("" + (i + 1) + " ");
            }
        }
        System.out.println();
        for (int i = 0; i < n; ++i) {
            final int I = i;
            dfa.get(i).forEach((c, to) -> {
                System.out.println(String.format("%d %d %c", I + 1, to + 1, c));
            });
        }
    }
}
