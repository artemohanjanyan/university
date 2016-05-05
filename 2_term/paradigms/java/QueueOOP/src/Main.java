import java.util.*;

/**
 * Created by artem on 05/03/15.
 */
public class Main {
    public static class Vertex {
        public List<Vertex> edges = new ArrayList<Vertex>(1);
        public boolean used = false;
        public int distance = -1;
    }

    public static Vertex[] solve(String ...args) {
        int n = Integer.parseInt(args[0]);
        Vertex graph[] = new Vertex[n];
        for (int i = 0; i < n; ++i) {
            graph[i] = new Vertex();
        }
        Vertex start = graph[Integer.parseInt(args[1]) - 1];

        for (int i = 2; i < args.length; ++i) {
            String edge[] = args[i].split(" ");
            int a = Integer.parseInt(edge[0]) - 1;
            int b = Integer.parseInt(edge[1]) - 1;

            graph[a].edges.add(graph[b]);
            graph[b].edges.add(graph[a]);
        }

        Deque queue = new LinkedDeque();
        queue.enqueue(start);
        start.used = true;
        start.distance = 0;

        while (!queue.isEmpty()) {
            Vertex current = (Vertex) queue.dequeue();

            current.edges.stream().filter(to -> !to.used).forEach(to -> {
                queue.enqueue(to);
                to.used = true;
                to.distance = current.distance + 1;
            });
        }

        for (int i = 0; i < n; ++i) {
            System.out.println((i + 1) + ": " + graph[i].distance);
        }

        return graph;
    }

    public static void main(String args[]) {
        solve("5", "3", "1 2", "2 3", "3 4", "4 5");
    }
}
