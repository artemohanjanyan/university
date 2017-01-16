package ru.ifmo.pp;

import junit.framework.TestCase;

import java.util.Random;
import java.util.concurrent.Phaser;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Automated test of linearizability of multi-threaded bank implementation.
 *
 * @author Roman Elizarov
 */
public class LinearizabilityTest extends TestCase {
    private static final int N = 10; // that is a number of accounts bank is going to have
    private static final int RUN_ACCOUNTS = 3; // each run will touch this # of accounts
    private static final int RUNS = 1000;

    private static final int MIN_THREADS = 2;
    private static final int MAX_THREADS = 3;
    private static final int MAX_OPS_PER_THREAD = 2; // Max ops per threads
    private static final int MAX_OPS_PER_RUN = 5; // Max ops total NOTE:WARNING: Will try almost MAX_OPS_PER_RUN! (factorial) serial executions

    private static final int MIN_EXECUTIONS = 1000; // min executions per run
    private static final int MAX_EXECUTIONS = 1_000_000; // max executions per run (if not all results seen yet)

    private static final int MIN_FUZZ = 0;
    private static final int MAX_FUZZ = 100; // max # of CPU consuming spins before operations

    private static final int MIN_RESULTS = 3; // don't run trivial combos of operations

    private final Random rnd = new Random(20141101);
    private final Phaser phaser = new Phaser(1); // will register additional threads as they are created

    private final int[] runAccounts = new int[RUN_ACCOUNTS];
    private final long[] baseAmount = new long[RUN_ACCOUNTS];

    private int allOpsCnt;
    private final Operation[] allOps = new Operation[MAX_OPS_PER_RUN];
    private final int[] threadOpsCnt = new int[MAX_THREADS];
    private final int[][] threadOps = new int[MAX_THREADS][MAX_OPS_PER_THREAD];

    private Bank bank;

    private int nThreads; // current number of threads
    private int runNo; // current run number
    private int needFuzz; // current fuzzing limit (if needed), up to MAX_FUZZ

    private final ResultsHash resultsHash = new ResultsHash();
    private final Results results = new Results(MAX_OPS_PER_RUN); // allocate results for MAX_OPS_PER_RUN

    private int sumTotalResults;
    private int sumSeenResults;

    public void testLinearizability() {
        for (nThreads = 1; nThreads <= MAX_THREADS; nThreads++) {
            phaser.register();
            new TestThread(nThreads - 1).start(); // add a thread
            if (nThreads < MIN_THREADS)
                continue;
            for (runNo = 1; runNo <= RUNS; runNo++)
                doOneRun();
        }
        dumpSumStats();
    }

    @Override
    protected void tearDown() throws Exception {
        phaser.forceTermination();
    }

    private void doOneRun() {
        createNonTrivialRun();
        boolean printedWorking = false;
        for (int i = 0; i < MAX_EXECUTIONS; i++) {
            if (i >= MIN_EXECUTIONS) {
                if (resultsHash.getSeenCount() == resultsHash.getTotalCount())
                    break; // break when already seen all and min executions done
                if (!printedWorking) {
                    printedWorking = true;
                    dumpRunStats("working");
                }
            }
            needFuzz = Math.min(MAX_FUZZ, MIN_FUZZ + i / MIN_EXECUTIONS);
            doOneExecution();
            if (!resultsHash.findResultsAndCountSeen(results)) {
                dumpNonLinearizableError();
                fail("Non-linearizable");
            }
        }
        dumpRunStats("completed");
        if (resultsHash.getSeenCount() < resultsHash.getTotalCount()) {
            dumpIncompleteWarning();
        }
        sumTotalResults += resultsHash.getTotalCount();
        sumSeenResults += resultsHash.getSeenCount();
    }

    private void dumpIncompleteWarning() {
        System.out.println();
        System.out.println("===================================================");
        System.out.println("WARNING: Run failed to produce all possible results");
        dumpRun(false);
    }

    private void dumpNonLinearizableError() {
        System.out.println();
        System.out.println("=======================================");
        System.out.println("ERROR: Non-linearizable execution found");
        dumpRun(true);
    }

    private void createNonTrivialRun() {
        int maxPossibleResults = 1;
        for (int i = 2; i <= nThreads; i++)
            maxPossibleResults *= i;
        do {
            createRandomRun();
            resultsHash.clear();
            results.setSize(allOpsCnt);
            serialScan(0, 0, new int[allOpsCnt]);
        } while (resultsHash.getTotalCount() < Math.min(MIN_RESULTS, maxPossibleResults));
    }

    private void createRandomRun() {
        for (int i = 0; i < RUN_ACCOUNTS; i++) {
            boolean ok;
            do {
                runAccounts[i] = rnd.nextInt(N);
                ok = true;
                for (int j = 0; j < i; j++)
                    if (runAccounts[i] == runAccounts[j])
                        ok = false;
            } while (!ok);
            baseAmount[i] = nextRndAmount();
        }
        allOpsCnt = 0;
        for (int t = 0; t < nThreads; t++) {
            int maxOpsCnt = Math.min(MAX_OPS_PER_THREAD, MAX_OPS_PER_RUN - allOpsCnt - (nThreads - t - 1));
            int opsCnt = rnd.nextInt(maxOpsCnt) + 1;
            threadOpsCnt[t] = opsCnt;
            for (int q = 0; q < opsCnt; q++) {
                Operation op;
                switch (rnd.nextInt(5)) {
                    case 0:
                        op = new Operation.GetAmount(nextRndRunAccount());
                        break;
                    case 1:
                        op = new Operation.GetTotalAmount();
                        break;
                    case 2:
                        op = new Operation.Deposit(nextRndRunAccount(), nextRndAmountOrInvalid());
                        break;
                    case 3:
                        op = new Operation.Withdraw(nextRndRunAccount(), nextRndAmountOrInvalid());
                        break;
                    case 4:
                        int i;
                        int j;
                        do {
                            i = nextRndRunAccount();
                            j = nextRndRunAccount();
                        } while (i == j);
                        op = new Operation.Transfer(i, j, nextRndAmountOrInvalid());
                        break;
                    default:
                        throw new AssertionError();
                }
                int k = allOpsCnt++;
                allOps[k] = op;
                threadOps[t][q] = k;
            }
        }
    }

    private void dumpRunStats(String state) {
        System.out.printf("\rUsing %d/%d threads run %d/%d " + state + ", seen %d out of %d results ...  ",
                nThreads, MAX_THREADS,
                runNo, RUNS,
                resultsHash.getSeenCount(), resultsHash.getTotalCount());
    }

    private void dumpSumStats() {
        System.out.printf("\rAll runs completed, seen %d out of %d (%.2f%%) results        %n",
                sumSeenResults, sumTotalResults, 100.0 * sumSeenResults / sumTotalResults);
    }

    private void dumpRun(boolean withRunResults) {
        System.out.println("Initial state:");
        for (int i = 0; i < RUN_ACCOUNTS; i++)
            System.out.println("  Account #" + runAccounts[i] + " with amount " + baseAmount[i]);
        System.out.println("Operations:");
        for (int t = 0; t < nThreads; t++)
            for (int q = 0; q < threadOpsCnt[t]; q++) {
                int k = threadOps[t][q];
                System.out.println("  [thread " + t + ", op " + q + "] " + allOps[k] +
                        (withRunResults ? " with result " + results.get(k) : ""));
            }
        System.out.println("All valid results:");
        resultsHash.dump();
    }

    private void serialScan(int i, int used, int[] order) {
        if (i >= allOpsCnt) {
            initBank(new SequentialBank(N));
            for (int k = 0; k < allOpsCnt; k++)
                results.set(order[k], allOps[order[k]].invoke(bank));
            resultsHash.registerResults(results);
            return;
        }
        for (int t = 0; t < nThreads; t++) {
            for (int q = 0; q < threadOpsCnt[t]; q++) {
                int k = threadOps[t][q];
                if ((used & (1 << k)) == 0) {
                    order[i] = k;
                    serialScan(i + 1, used | (1 << k), order);
                    break; // try next thread, thread ops only in order
                }
            }
        }
    }

    private void doOneExecution() {
        initBank(new BankImpl(N));
        phaser.arriveAndAwaitAdvance();
        phaser.arriveAndAwaitAdvance();
    }

    private void initBank(Bank bank) {
        this.bank = bank;
        for (int i = 0; i < RUN_ACCOUNTS; i++)
            this.bank.deposit(runAccounts[i], baseAmount[i]);
    }

    private int nextRndRunAccount() {
        return runAccounts[rnd.nextInt(RUN_ACCOUNTS)];
    }

    private long nextRndAmountOrInvalid() {
        if (rnd.nextInt(100) == 0) { // 1% of invalid amounts
            switch (rnd.nextInt(6)) {
                case 0: return 0;
                case 1: return -1;
                case 2: return Long.MIN_VALUE;
                case 3: return Bank.MAX_AMOUNT + 1;
                case 4: return Bank.MAX_AMOUNT + 2;
                case 5: return Long.MAX_VALUE;
            }
        }
        return nextRndAmount();
    }

    private long nextRndAmount() {
        int base = 1_000_000_000;
        return 1 + rnd.nextInt(base) + rnd.nextInt((int)(Bank.MAX_AMOUNT / base)) * (long)base;
    }

    private class TestThread extends Thread {
        private final int threadNo;
        private final AtomicInteger cpuConsumer = new AtomicInteger();

        public TestThread(int threadNo) {
            super("TestThread-" + threadNo);
            this.threadNo = threadNo;
        }

        @Override
        public void run() {
            while (true) {
                if (phaser.arriveAndAwaitAdvance() < 0)
                    return;
                fuzzIfNeeded();
                doOneExecution();
                if (phaser.arriveAndAwaitAdvance() < 0)
                    return;
            }
        }

        private void doOneExecution() {
            int q = 0;
            while (true) {
                int k = threadOps[threadNo][q];
                results.set(k, allOps[k].invoke(bank));
                if (++q >= threadOpsCnt[threadNo])
                    break;
                fuzzIfNeeded();
            }
        }

        private void fuzzIfNeeded() {
            if (needFuzz > 0)
                fuzz();
        }

        private void fuzz() {
            int fuzz = ThreadLocalRandom.current().nextInt(needFuzz);
            for (int i = 0; i < fuzz; i++)
                consumeCPU();
        }

        private void consumeCPU() {
            cpuConsumer.incrementAndGet();
        }
    }
}