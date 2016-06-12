package ru.ifmo.ctddev.ohanjanyan.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
    private List<E> objects = null;
    private Comparator<? super E> comparator = null;

    public ArraySet() {
        this((Comparator<E>) null);
    }

    public ArraySet(Comparator<? super E> comparator) {
        objects = Collections.emptyList();
        this.comparator = comparator;
    }

    public ArraySet(Collection<E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;

        TreeSet<E> treeSet = new TreeSet<>(comparator);
        //noinspection Convert2streamapi
        for (E elem : collection) {
            treeSet.add(elem);
        }

        this.objects = new ArrayList<>(treeSet);
    }

    private ArraySet(List<E> objects, Comparator<? super E> comparator) {
        this.objects = objects;
        this.comparator = comparator;
    }

    //
    // AbstractSet
    //

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return Collections.binarySearch(objects, (E) o, comparator) >= 0;
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableList(objects).iterator();
    }

    private E get(int i) {
        if (i >= size() || i < 0) {
            throw new IndexOutOfBoundsException();
        }
        return objects.get(i);
    }

    @Override
    public int size() {
        return objects == null ? 0 : objects.size();
    }

    //
    // SortedSet
    //

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public E first() {
        try {
            return get(0);
        } catch (IndexOutOfBoundsException e) {
            throw new NoSuchElementException();
        }
    }

    @Override
    public E last() {
        try {
            return get(size() - 1);
        } catch (IndexOutOfBoundsException e) {
            throw new NoSuchElementException();
        }
    }

//    /**
//     * @param l such that <code>predicate(l) == false</code> (possibly nonexistent, not tested)
//     * @param r such that <code>predicate(r) == true</code> (possibly nonexistent, not tested)
//     * @param predicate monotonous predicate
//     * @return min i: <code>predicate(i) == true</code>
//     */
//    static private int binarySearch(int l, int r, Predicate<Integer> predicate) {
//        while (l + 1 < r) {
//            int mid = l + (r - l) / 2;
//            if (predicate.test(mid)) {
//                r = mid;
//            } else {
//                l = mid;
//            }
//        }
//
//        return r;
//    }

    //
    // NavigableSet
    //

    @Override
    public E lower(E e) {
        int index = findToI(e, false);
        return index >= 0 ? objects.get(index) : null;
    }

    @Override
    public E floor(E e) {
        int index = findToI(e, true);
        return index >= 0 ? objects.get(index) : null;
    }

    @Override
    public E ceiling(E e) {
        int index = findFromI(e, true);
        return index < objects.size() ? objects.get(index) : null;
    }

    @Override
    public E higher(E e) {
        int index = findFromI(e, false);
        return index < objects.size() ? objects.get(index) : null;
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(objects instanceof DescendingList ?
                ((DescendingList<E>) objects).objects :
                new DescendingList<>(objects), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        int fromI = findFromI(fromElement, fromInclusive);
        int toI = findToI(toElement, toInclusive) + 1;
        if (toI < fromI) {
            toI = fromI;
        }
        return new ArraySet<>(objects.subList(fromI, toI), comparator);
    }

    @Override
    public NavigableSet<E> headSet(E toElement, boolean inclusive) {
        int toI = findToI(toElement, inclusive) + 1;
        return new ArraySet<>(objects.subList(0, toI), comparator);
    }

    @Override
    public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
        int fromI = findFromI(fromElement, inclusive);
        return new ArraySet<>(objects.subList(fromI, objects.size()), comparator);
    }

    private int findFromI(E fromElement, boolean inclusive) {
        int index = Collections.binarySearch(objects, fromElement, comparator);
        if (index < 0) {
            index = ~index;
        } else if (!inclusive) {
            ++index;
        }
        return index;
    }

    private int findToI(E toElement, boolean inclusive) {
        int index = Collections.binarySearch(objects, toElement, comparator);
        if (index < 0) {
            index = ~index - 1;
        } else if (!inclusive) {
            --index;
        }
        return index;
    }

    private static class DescendingList<E> extends AbstractList<E> implements RandomAccess {
        public List<E> objects;

        public DescendingList(List<E> objects) {
            this.objects = objects;
        }

        @Override
        public E get(int index) {
            return objects.get(objects.size() - 1);
        }

        @Override
        public int size() {
            return objects.size();
        }
    }
}
