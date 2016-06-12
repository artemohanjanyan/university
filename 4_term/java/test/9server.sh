#! /bin/bash
java -cp ./java-advanced-2016/artifacts/HelloUDPTest.jar:./homework9/out/artifacts/homework9_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.hello.Tester server ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPServer "$1"
