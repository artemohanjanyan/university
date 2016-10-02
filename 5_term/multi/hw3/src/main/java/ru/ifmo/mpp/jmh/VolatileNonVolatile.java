package ru.ifmo.mpp.jmh;

import org.openjdk.jmh.annotations.*;

public class VolatileNonVolatile {

    // Volatile, shared

    @State(Scope.Group)
    public static class VolatileSharedState {
        volatile int var;
    }

    @Benchmark
    @Group("volatile_shared")
    public int volatileSharedReader(VolatileSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("volatile_shared")
    public void volatileSharedWriter(VolatileSharedState s) {
        ++s.var;
    }

    // Non-volatile, shared

    @State(Scope.Group)
    public static class NonVolatileSharedState {
        int var;
    }

    @Benchmark
    @Group("non_volatile_shared")
    public int nonVolatileSharedReader(NonVolatileSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("non_volatile_shared")
    public void nonVolatileSharedWriter(NonVolatileSharedState s) {
        ++s.var;
    }

    // Volatile, not shared

    @State(Scope.Benchmark)
    public static class VolatileNotSharedState {
        volatile int var;
    }

    @Benchmark
    @Group("volatile_not_shared")
    public int volatileNotSharedReader(VolatileNotSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("volatile_not_shared")
    public void volatileNotSharedWriter(VolatileNotSharedState s) {
        ++s.var;
    }

    // Non-volatile, not shared

    @State(Scope.Benchmark)
    public static class NonVolatileNotSharedState {
        int var;
    }

    @Benchmark
    @Group("non_volatile_not_shared")
    public int nonVolatileNotSharedreader(NonVolatileNotSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("non_volatile_not_shared")
    public void nonVolatileNotSharedwriter(NonVolatileNotSharedState s) {
        ++s.var;
    }
}
