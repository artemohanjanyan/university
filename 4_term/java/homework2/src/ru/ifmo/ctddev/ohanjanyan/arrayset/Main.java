package ru.ifmo.ctddev.ohanjanyan.arrayset;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.TreeSet;

public class Main {
    public static void main(String[] args) {
        ArraySet<Integer> set = new ArraySet<>(
                Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        );
        System.out.println(Arrays.asList(set.descendingSet().toArray()));

        TreeSet<Integer> set1 = new TreeSet<>(
                Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        );
        System.out.println(Arrays.asList(set1.descendingSet().toArray()));

        ArraySet<Integer> arraySet = new ArraySet<>();
        System.out.println(arraySet.descendingSet().toArray().length);
    }
}
