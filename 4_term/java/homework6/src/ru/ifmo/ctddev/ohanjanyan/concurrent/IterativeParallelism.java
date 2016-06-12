package ru.ifmo.ctddev.ohanjanyan.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.*;

/**
 * Class provides implementations of {@link ScalarIP} and {@link ListIP} interfaces.
 */
public class IterativeParallelism implements ScalarIP, ListIP {
    private ParallelMapper parallelMapper;

    /**
     * Creates an instance of {@link IterativeParallelism}.
     */
    public IterativeParallelism() {
        parallelMapper = null;
    }

    /**
     * Creates an instance of {@link IterativeParallelism}.
     * @param parallelMapper {@link ParallelMapper} to be used for concurrency.
     */
    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    /**
     * Finds maximum element in the list.
     * @param threadN number of threads to use.
     * @param list the list to be searched.
     * @param comparator comparator to be used for searching.
     * @param <T> type of elements in the list.
     * @return maximum of list, or {@code null} if list is empty.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T> T maximum(int threadN, List<? extends T> list, Comparator<? super T> comparator)
            throws InterruptedException {
        if (list.isEmpty()) {
            return null;
        } else {
            BinaryOperator<T> fold = (a, b) -> comparator.compare(a, b) < 0 ? b : a;
            return parFold(threadN, list, () -> list.get(0), fold, fold);
        }
    }

    /**
     * Finds minimum element in the list.
     * @param threadN number of threads to use.
     * @param list the list to be searched.
     * @param comparator comparator to be used for searching.
     * @param <T> type of elements in the list.
     * @return minimum of list, or {@code null} if list is empty.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T> T minimum(int threadN, List<? extends T> list, Comparator<? super T> comparator)
            throws InterruptedException {
        return maximum(threadN, list, comparator.reversed());
    }

    /**
     * Tests if all list elements satisfy the predicate.
     * @param threadN number of threads to use.
     * @param list the list to be tested.
     * @param predicate predicate to test with.
     * @param <T> type of elements in the list.
     * @return {@code true} if all elements of the list satisfy predicate, {@code false} otherwise.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T> boolean all(int threadN, List<? extends T> list, Predicate<? super T> predicate)
            throws InterruptedException {
        return parFold(threadN, list, () -> true, (acc, a) -> acc && predicate.test(a), (a, b) -> a && b);
    }

    /**
     * Tests if any list element satisfies the predicate.
     * @param threadN number of threads to use.
     * @param list the list to be tested.
     * @param predicate predicate to test with.
     * @param <T> type of elements in the list.
     * @return {@code true} if any element of the list satisfies predicate, {@code false} otherwise.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T> boolean any(int threadN, List<? extends T> list, Predicate<? super T> predicate)
            throws InterruptedException {
        return !all(threadN, list, predicate.negate());
    }

    /**
     * Concatenates list elements into a string.
     * @param threadN number of threads to use.
     * @param values the list to be joined.
     * @return Concatenated string representations of list elements.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public String join(int threadN, List<?> values) throws InterruptedException {
        return parFold(threadN, values, StringBuilder::new,
                (acc, a) -> {
                    acc.append(a.toString());
                    return acc;
                },
                (a, b) -> {
                    a.append(b);
                    return a;
                }).toString();
    }

    /**
     * Filters given list by predicate and returns filtered list.
     * @param threadN number of threads to use.
     * @param values the list to be filtered.
     * @param predicate predicate to test list elements with.
     * @param <T> type of elements in the list.
     * @return list consisting of elements of given list which satisfy the predicate.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T> List<T> filter(final int threadN, final List<? extends T> values,
                              final Predicate<? super T> predicate) throws InterruptedException {
        return parFold(threadN, values, ArrayList<T>::new,
                (acc, elem) -> {
                    if (predicate.test(elem)) {
                        acc.add(elem);
                    }
                    return acc;
                },
                combinerBuilder());
    }

    /**
     * Applies given function on every element of list, and creates list of results of function applications.
     * @param threadN number of threads to use.
     * @param values the list to be mapped.
     * @param function function to apply on list elements.
     * @param <T> type of elements of the initial list.
     * @param <U> type of elements of the resulting list.
     * @return list of results of function applications.
     * @throws InterruptedException if one of created threads was interrupted.
     */
    @Override
    public <T, U> List<U> map(final int threadN, final List<? extends T> values,
                              final Function<? super T, ? extends U> function) throws InterruptedException {
        return parFold(threadN, values, ArrayList<U>::new,
                (acc, elem) -> {
                    acc.add(function.apply(elem));
                    return acc;
                },
                combinerBuilder());
    }

    private void parallelExecute(int threadN, Function<Integer, Runnable> runnableFactory) throws InterruptedException {
        Thread threads[] = new Thread[threadN - 1];
        for (int i = 1; i < threadN; ++i) {
            threads[i - 1] = new Thread(runnableFactory.apply(i));
            threads[i - 1].start();
        }
        runnableFactory.apply(0).run();
        for (int i = 1; i < threadN; ++i) {
            threads[i - 1].join();
        }
    }

    private <T, U> U parFold(int threadN, List<? extends T> list,
                             Supplier<U> zeroSupplier, BiFunction<U, ? super T, U> fold,
                             BinaryOperator<U> ansFold)
            throws InterruptedException {
        List<U> ansList;

        if (parallelMapper == null) {
            ansList = new ArrayList<>(Collections.nCopies(threadN, null));

            parallelExecute(threadN, i -> () -> ansList.set(i,
                    subList(list, i, threadN)
                            .stream()
                            .reduce(zeroSupplier.get(), fold, ansFold)
            ));
        } else {
            List<List<? extends T>> argList = new ArrayList<>(Collections.nCopies(threadN, null));
            for (int i = 0; i < threadN; ++i) {
                argList.set(i, subList(list, i, threadN));
            }
            ansList = parallelMapper.map((l) -> l.stream().reduce(zeroSupplier.get(), fold, ansFold), argList);
        }

        return ansList.stream().reduce(zeroSupplier.get(), ansFold);
    }

    private static <T> List<T> subList(List<T> list, int i, int threadN) {
        return list.subList(mul(list.size(), i, threadN), mul(list.size(), i + 1, threadN));
    }

    /**
     * Multiplies {@code a} by fraction {@code b/c}.
     * @param a first multiplier
     * @param b enumerator of second multiplier
     * @param c denominator of second multiplier
     * @return a * b / c
     */
    private static int mul(int a, int b, int c) {
        return (int) ((long) a * b / c);
    }

    private static <E> BinaryOperator<List<E>> combinerBuilder() {
        return (a, b) -> {
            a.addAll(b);
            return a;
        };
    }
}
