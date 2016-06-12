package ru.ifmo.ctddev.ohanjanyan.crawler;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

/**
 * Class which manages several threads and executes supplied tasks concurrently on those threads.
 */
public class ThreadPool implements AutoCloseable {
    private final Queue<Runnable> queue;
    private final List<Thread> threads;
    private final int threadN;
    private final Runnable runnable;

    /**
     * Creates an instance of {@link ThreadPool}.
     * @param threadN maximum number of threads to be created.
     */
    public ThreadPool(int threadN) {
        this.threadN = threadN;
        queue = new ArrayDeque<>();
        threads = new ArrayList<>();
        runnable = () -> {
            while (true) {
                Runnable threadTask;
                synchronized (queue) {
                    while (queue.isEmpty()) {
                        try {
                            queue.wait();
                        } catch (InterruptedException e) {
                            return;
                        }
                    }
                    threadTask = queue.remove();
                }

                try {
                    threadTask.run();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
    }

    /**
     * Adds task to execution queue. This task will be executed after all tasks added before this one will start
     * executing and some thread will become free. <br>
     * This method is thread-safe, one can call it from different threads simultaneously.
     * @param task task to enqueue.
     */
    public void pushTask(Runnable task) {
        synchronized (queue) {
            queue.add(task);

            if (queue.size() > threads.size() && threads.size() < threadN) {
                threads.add(new Thread(runnable));
                threads.get(threads.size() - 1).start();
            }

            if (queue.size() == 1) {
                queue.notify();
            }
        }
    }

    /**
     * Closes all running threads and waits until they stop.
     * @throws InterruptedException if any thread has interrupted the current thread.
     * The interrupted status of the current thread is cleared when this exception is thrown.
     */
    @Override
    public void close() throws InterruptedException {
        threads.forEach(Thread::interrupt);
        for (Thread thread : threads) {
            thread.join();
        }
    }
}
