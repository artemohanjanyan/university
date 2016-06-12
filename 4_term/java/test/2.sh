#! /bin/bash
java -cp ./java-advanced-2016/artifacts/ArraySetTest.jar:./homework2/out/artifacts/homework2_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.arrayset.Tester NavigableSet ru.ifmo.ctddev.ohanjanyan.arrayset.ArraySet $1
