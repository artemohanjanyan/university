#! /bin/bash
java -cp ./java-advanced-2016/artifacts/WalkTest.jar:./homework1/out/artifacts/homework1_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.walk.Tester RecursiveWalk ru.ifmo.ctddev.ohanjanyan.walk.RecursiveWalk $1
