#! /bin/bash
java -cp ./java-advanced-2016/artifacts/ParallelMapperTest.jar:./homework6/out/artifacts/homework7_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.mapper.Tester list \
	ru.ifmo.ctddev.ohanjanyan.concurrent.ParallelMapperImpl,ru.ifmo.ctddev.ohanjanyan.concurrent.IterativeParallelism "$1"
