// Not mine

import java.io.*;
import java.util.*;

public class Turing {
    private static Map<State, State> transition = new HashMap<>();

    public static void main(String[] args) throws IOException {
        boolean info = false;
        PrintWriter writer = new PrintWriter(System.out);
        try {
            if (args.length >= 4) {
                if (args[3].equals("-info")) {
                    info = true;
                } else {
                    writer.println("Usage: <Tape> <Machine> <Result> [-info [<Out File info>]]");
                    return;
                }
                if (args.length == 5) {
                    writer = new PrintWriter(args[4]);
                }
            } else if (args.length != 3) {
                writer.println("Usage: <Tape> <Machine> <Result> [-info [<Out File info>]]");
                return;
            }
            Scanner scanner = new Scanner(new File(args[0]));
            Deque<String> tape = new ArrayDeque<>();
            if (scanner.hasNext()) {
                tape = new ArrayDeque<>(Arrays.asList(scanner.nextLine().replaceAll("\\s", "").split("")));
            }
            scanner = new Scanner(new File(args[2]));
            String finalState = scanner.hasNext() ? scanner.nextLine().trim().toUpperCase() : "";
            if (!(finalState.equals("AC") || finalState.equals("RJ"))) {
                writer.println("Incorrect result format, Usage first line: AC or RJ, second line: final tape or empty if final tape no matter");
                return;
            }
            Deque<String> finalTape = null;
            if (scanner.hasNext()) {
                finalTape = new ArrayDeque<>(Arrays.asList(scanner.nextLine().replaceAll("\\s", "").split("")));
            }
            scanner = new Scanner(new File(args[1]));
            if (!scanner.hasNextInt()) {
                writer.println("Use number of states first in machine description");
                return;
            }
            int nTapes = scanner.nextInt();
            scanner.nextLine();
            int i = 2;
            if (transition.isEmpty()) {
                while (scanner.hasNext()) {
                    String[] string = scanner.nextLine().split("\\s+");
                    if (string.length != 3 * nTapes + 3 || !string[nTapes + 1].equals("->")) {
                        if (string.length == 0 || string.length == 1 && string[0].isEmpty()) {
                            continue;
                        }
                        writer.println("Can't parse machine, line " + i);
                        for (String ss : string) {
                            writer.print(ss + " ");
                        }
                        return;
                    }
                    List<String> symbols = new ArrayList<>();
                    List<Integer> moves = new ArrayList<>();
                    symbols.addAll(Arrays.asList(string).subList(1, nTapes + 1));
                    State start = new State(string[0], symbols, null);
                    if (transition.containsKey(start)) {
                        writer.println("Ambiguous define line " + i);
                        return;
                    }
                    symbols = new ArrayList<>();
                    for (int j = nTapes + 3; j < 3 * nTapes + 3; j += 2) {
                        symbols.add(string[j]);
                        int move;
                        switch (string[j + 1]) {
                            case "<":
                                move = -1;
                                break;
                            case "^":
                                move = 0;
                                break;
                            case ">":
                                move = 1;
                                break;
                            default:
                                writer.println("Can't parse machine, line " + i);
                                for (String ss : string) {
                                    writer.print(ss + " ");
                                }
                                return;
                        }
                        moves.add(move);
                    }
                    State end = new State(string[nTapes + 2], symbols, moves);
                    transition.put(start, end);
                    i++;
                }
            }
            List<Tape> tapes = new ArrayList<>();
            String state = "S";
            tapes.add(new Tape(new ArrayDeque<>(), tape));
            for (int j = 1; j < nTapes; j++) {
                tapes.add(new Tape(new ArrayDeque<>(), new ArrayDeque<>()));
            }
            if (info) {
                writer.println("Before: state S");
                for (int j = 0; j < nTapes; j++) {
                    writer.println("Tape " + (j + 1) + ": " + tapes.get(j).toString());
                }
                writer.println();
            }
            for (int j = 1; j <= 10_000_000; j++) {
                List<String> symbols = new ArrayList<>();
                for (int k = 0; k < nTapes; k++) {
                    symbols.add(tapes.get(k).get());
                }
                State start = new State(state, symbols, null);
                State end = transition.get(start);
                if (end == null) {
                    writer.print("On " + j + " step not found transition {State: " + state + " symbols:");
                    for (String ss : symbols) {
                        writer.print(" " + ss);
                    }
                    writer.println("}");
                } else {
                    state = end.state;
                    for (int k = 0; k < nTapes; k++) {
                        tapes.get(k).set(end.symbols.get(k));
                        tapes.get(k).move(end.moves.get(k));
                    }
                    if (info) {
                        writer.println("Step " + j + " : state " + state);
                        for (int k = 0; k < nTapes; k++) {
                            writer.println("Tape " + (k + 1) + ": " + tapes.get(k).toString());
                        }
                        writer.println();
                    }
                    switch (end.state) {
                        case "RJ":
                            if (finalState.equals("RJ")) {
                                writer.println("OK " + j + " steps");
                            } else {
                                writer.println("Wrong final state");
                            }
                            break;
                        case "AC":
                            if (finalState.equals("AC")) {
                                if (finalTape == null) {
                                    writer.println("OK " + j + " steps");
                                } else {
                                    Tape tape1 = tapes.get(0);
                                    if (tape1.first.isEmpty() && tape1.second.size() == finalTape.size()) {
                                        while (!finalTape.isEmpty()) {
                                            if (tape1.get().equals(finalTape.removeFirst())) {
                                                tape1.right();
                                            } else {
                                                writer.println("Correct final state, but wrong final tape");
                                                break;
                                            }
                                        }
                                        if (finalTape.isEmpty()) {
                                            writer.println("OK " + j + " steps");
                                        }
                                    } else {
                                        writer.println("Correct final state, but wrong final tape");
                                    }
                                }
                            } else {
                                writer.println("Wrong final state");
                            }
                            break;
                        default:
                            continue;
                    }
                }
                return;
            }
            writer.println("Too more steps");
        } finally {
            writer.flush();
        }
    }

    private static class State {
        String state;
        List<String> symbols;
        List<Integer> moves;

        State(String state, List<String> symbols, List<Integer> moves) {
            this.state = state;
            this.symbols = symbols;
            this.moves = moves;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof State) {
                State st = (State) obj;
                if (!state.equals(st.state)) {
                    return false;
                }
                for (int i = 0; i < symbols.size(); i++) {
                    if (!symbols.get(i).equals(st.symbols.get(i))) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            int hash = state.hashCode();
            for (int i = 0; i < symbols.size(); i++) {
                hash ^= symbols.get(i).hashCode() * (i + 1);
            }
            return hash;
        }
    }

    private static class Tape {
        Deque<String> first;
        Deque<String> second;

        Tape(Deque<String> first, Deque<String> second) {
            this.first = first;
            this.second = second;
        }

        void left() {
            if (first.isEmpty()) {
                first.addLast("_");
            }
            second.addFirst(first.removeLast());
        }

        void right() {
            first.addLast(second.removeFirst());
        }

        void move(int m) {
            if (m == -1) {
                left();
            } else if (m == 1) {
                right();
            }
            trim();
        }

        String get() {
            if (second.isEmpty()) {
                second.addFirst("_");
            }
            return second.getFirst();
        }

        void set(String s) {
            second.removeFirst();
            second.addFirst(s);
        }

        @Override
        public String toString() {
            StringBuilder res = new StringBuilder();
            first.forEach(e -> res.append(e).append(" "));
            res.append("# ");
            second.forEach(e -> res.append(e).append(" "));
            return res.toString();
        }

        void trim() {
            while (!first.isEmpty() && first.getFirst().equals("_")) {
                first.removeFirst();
            }
            while (!second.isEmpty() && second.getLast().equals("_")) {
                second.removeLast();
            }
        }
    }
}
