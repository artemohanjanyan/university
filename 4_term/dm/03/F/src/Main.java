import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Locale;
import java.util.StringTokenizer;

public class Main {

    private class Poly {
        BigInteger a[];

        Poly(int n) {
            a = new BigInteger[n];
        }

        BigInteger get(int x) {
            BigInteger bigX = new BigInteger("" + x);
            BigInteger accX = BigInteger.ONE;
            BigInteger ans = BigInteger.ZERO;
            for (BigInteger anA : a) {
                ans = ans.add(anA.multiply(accX));
                accX = accX.multiply(bigX);
            }
            return ans;
        }
    }

    private void solve() throws IOException {
        int n = nextInt();

        int p[] = new int[n];
        for (int i = 0; i < n; ++i) {
            p[i] = nextInt();
        }

        Poly poly[] = new Poly[n];

        for (int i = 0; i < n; ++i) {
            int m = nextInt();
            poly[i] = new Poly(m + 1);
            for (int j = 0; j <= m; ++j) {
                poly[i].a[m - j] = new BigInteger(nextToken());
            }
        }

        int d = nextInt();
        boolean graph[][] = new boolean[n][n];
        for (int i = 0; i < d; ++i) {
            int a = nextInt() - 1;
            int b = nextInt() - 1;
            graph[a][b] = true;
        }

        int childrenN[] = new int[n];
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (graph[i][j]) {
                    ++childrenN[i];
                }
            }
        }

        boolean used[] = new boolean[n];
        int time = 0;
        for (int i = 0; i < n; ++i) {
            time += p[i];
        }

        int schedule[] = new int[n];

        BigInteger maxF = null;

        for (int k = n - 1; k >= 0; --k) {
            BigInteger minF = null;
            int j = -1;
            for (int i = 0; i < n; ++i) {
                if (!used[i] && childrenN[i] == 0) {
                    BigInteger f = poly[i].get(time);
                    if (minF == null || f.compareTo(minF) < 0) {
                        minF = f;
                        j = i;
                    }
                }
            }
            used[j] = true;
            if (maxF == null || minF != null && maxF.compareTo(minF) < 0) {
                maxF = minF;
            }
            childrenN[j] = Integer.MAX_VALUE;

            time -= p[j];
            schedule[j] = time;

            for (int i = 0; i < n; ++i) {
                if (graph[i][j]) {
                    --childrenN[i];
                }
            }
        }

        out.println(maxF);
        for (int taskTime : schedule) {
            out.print(taskTime);
            out.print(" ");
        }
    }

    private void run() {
        try {
            //noinspection SpellCheckingInspection
            String fileName = "p1precfmax";
            br = new BufferedReader(new FileReader(fileName + ".in"));
            out = new PrintWriter(fileName + ".out");

            solve();

            out.close();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private BufferedReader br;
    private StringTokenizer in;
    private PrintWriter out;

    private String nextToken() throws IOException {
        while (in == null || !in.hasMoreTokens()) {
            in = new StringTokenizer(br.readLine());
        }
        return in.nextToken();
    }

    private int nextInt() throws IOException {
        return Integer.parseInt(nextToken());
    }

    public static void main(String[] args) throws IOException {
        Locale.setDefault(Locale.US);
        new Main().run();
    }

}