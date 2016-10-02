package ru.ifmo.mpp.jmh;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Group;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;

import java.util.concurrent.locks.ReentrantLock;

public class Synchronization {

    // Volatile, shared

    @State(Scope.Group)
    public static class VolatileSharedState {
        volatile long var;
    }

    @Benchmark
    @Group("volatile_shared")
    public long volatileSharedReader(VolatileSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("volatile_shared")
    public void volatileSharedWriter(VolatileSharedState s) {
        ++s.var;
    }


    // Synchronized, shared

    @State(Scope.Group)
    public static class SynchronizedSharedState {
        long var;
    }

    @Benchmark
    @Group("synchronized_shared")
        public long synchronizedSharedReader(SynchronizedSharedState s) {
        synchronized (s) {
            return s.var;
        }
    }

    @Benchmark
    @Group("synchronized_shared")
    public void synchronizedSharedWriter(SynchronizedSharedState s) {
        synchronized (s) {
            ++s.var;
        }
    }


    // ReentrantLock, shared

    @State(Scope.Group)
    public static class ReentrantLockSharedState {
        final ReentrantLock lock = new ReentrantLock();
        long var;
    }

    @Benchmark
    @Group("reentrant_lock_shared")
    public long reentrantLockSharedReader(ReentrantLockSharedState s) {
        s.lock.lock();
        long var = s.var;
        s.lock.unlock();
        return var;
    }

    @Benchmark
    @Group("reentrant_lock_shared")
    public void reentrantLockSharedWriter(ReentrantLockSharedState s) {
        s.lock.lock();
        ++s.var;
        s.lock.unlock();
    }


    // Volatile, not shared

    @State(Scope.Thread)
    public static class VolatileNotSharedState {
        volatile long var;
    }

    @Benchmark
    @Group("volatile_not_shared")
    public long volatileNotSharedReader(VolatileNotSharedState s) {
        return s.var;
    }

    @Benchmark
    @Group("volatile_not_shared")
    public void volatileNotSharedWriter(VolatileNotSharedState s) {
        ++s.var;
    }


    // Synchronized, not shared

    @State(Scope.Group)
    public static class SynchronizedNotSharedState {
        long var;
    }

    @Benchmark
    @Group("synchronized_not_shared")
    public long synchronizedNotSharedReader(SynchronizedNotSharedState s) {
        synchronized (s) {
            return s.var;
        }
    }

    @Benchmark
    @Group("synchronized_not_shared")
    public void synchronizedNotSharedWriter(SynchronizedNotSharedState s) {
        synchronized (s) {
            ++s.var;
        }
    }


    // ReentrantLock, not shared

    @State(Scope.Thread)
    public static class ReentrantLockNotSharedState {
        final ReentrantLock lock = new ReentrantLock();
        long var;
    }

    @Benchmark
    @Group("reentrant_lock_not_shared")
    public long reentrantLockNotSharedReader(ReentrantLockNotSharedState s) {
        s.lock.lock();
        long var = s.var;
        s.lock.unlock();
        return var;
    }

    @Benchmark
    @Group("reentrant_lock_not_shared")
    public void reentrantLockNotSharedWriter(ReentrantLockNotSharedState s) {
        s.lock.lock();
        ++s.var;
        s.lock.unlock();
    }
}
