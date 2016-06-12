#! /bin/bash
java -cp ./java-advanced-2016/artifacts/IterativeParallelismTest.jar:./homework6/out/artifacts/homework6_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.concurrent.Tester list ru.ifmo.ctddev.ohanjanyan.concurrent.IterativeParallelism "$1"
