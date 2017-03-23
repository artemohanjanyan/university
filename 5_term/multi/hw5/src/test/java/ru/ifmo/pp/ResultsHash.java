package ru.ifmo.pp;

import java.util.Arrays;

/**
 * Hash table of all encountered results.
 *
 * @author Roman Elizarov
 */
class ResultsHash {
    private static final int SIZE = 1024; // must be 2^N;

    private final Results[] hash = new Results[SIZE];
    private int totalCount;
    private int seenCount;


    void clear() {
        Arrays.fill(hash, null);
        totalCount = 0;
        seenCount = 0;
    }

    int getTotalCount() {
        return totalCount;
    }

    int getSeenCount() {
        return seenCount;
    }

    void registerResults(Results results) {
        int hIndex = results.hashCode() & (SIZE - 1);
        while (true) {
            Results hRes = hash[hIndex];
            if (hRes == null) {
                hRes = new Results(results);
                hash[hIndex] = hRes;
                totalCount++;
                break;
            }
            if (hRes.equals(results))
                break;
            if (hIndex == 0)
                hIndex = SIZE;
            hIndex--;
        }
    }

    boolean findResultsAndCountSeen(Results results) {
        int hIndex = results.hashCode() & (SIZE - 1);
        while (true) {
            Results hRes = hash[hIndex];
            if (hRes == null)
                return false;
            if (hRes.equals(results)) {
                if (hRes.getCount() == 0)
                    seenCount++;
                hRes.incCount();
                return true;
            }
            if (hIndex == 0)
                hIndex = SIZE;
            hIndex--;
        }
    }

    void dump() {
        for (Results results : hash)
            if (results != null)
                System.out.println("  " + results + " seen " + results.getCount() + " times");
    }
}
