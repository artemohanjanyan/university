package ru.ifmo.ctddev.ohanjanyan.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

/**
 * Provides implementation for {@link ParallelMapper}.
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final Thread[] threads;

    private final Queue<Runnable> runnableQueue = new LinkedList<>();

    private volatile int addRequestCount = 0;
    private final Object addRequest = new Object();

    /**
     * Creates an instance of {@link ParallelMapper}.
     * Creates {@code threadN} threads, which will be used in {@link #map}.
     * @param threadN number of threads to create.
     */
    public ParallelMapperImpl(int threadN) {
        threads = new Thread[threadN];

        for (int i = 0; i < threads.length; ++i) {
            threads[i] = new Thread(() -> {
                while (true) {
                    Runnable localRunnable;

                    synchronized (addRequest) {
                        ++addRequestCount;
                        addRequest.notify();
                    }

                    synchronized (runnableQueue) {
                        while (runnableQueue.isEmpty()) {
                            try {
                                if (!Thread.interrupted()) {
                                    runnableQueue.wait();
                                } else {
                                    return;
                                }
                            } catch (InterruptedException e) {
                                return;
                            }
                        }

                        localRunnable = runnableQueue.remove();
                    }

                    localRunnable.run();
                }
            });
            threads[i].start();
        }
    }

    /**
     * Closes all threads.
     * @throws InterruptedException if main thread is interrupted during {@link Thread#join()} call.
     */
    @Override
    public void close() throws InterruptedException {
        for (Thread thread : threads) {
            thread.interrupt();
        }
        for (Thread thread : threads) {
            thread.join();
        }
    }

    /**
     * Applies function on each element of list and returns list of results.
     * @param function function to apply.
     * @param list arguments list.
     * @param <T> type of arguments.
     * @param <R> type of results.
     * @return list of results of function applications.
     * @throws InterruptedException
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> function, List<? extends T> list)
            throws InterruptedException {
        ArrayList<R> resultList = new ArrayList<>(Collections.nCopies(list.size(), null));

        final TaskInfo taskInfo = new TaskInfo();

        for (int i = 0; i < list.size(); ++i) {
            final int elementId = i;

            synchronized (addRequest) {
                while (addRequestCount == 0) {
                    addRequest.wait();
                }
                --addRequestCount;
            }

            synchronized (runnableQueue) {
                runnableQueue.add(() -> {
                    resultList.set(elementId, function.apply(list.get(elementId)));

                    synchronized (taskInfo) {
                        if (++taskInfo.readyCount == list.size()) {
                            taskInfo.notify();
                        }
                    }
                });
                runnableQueue.notify();
            }
        }

        synchronized (taskInfo) {
            while (taskInfo.readyCount < list.size()) {
                taskInfo.wait();
            }
        }

        return resultList;
    }

    private static class TaskInfo {
        private volatile int readyCount = 0;
    }
}
