#! /bin/bash
java -cp ./java-advanced-2016/artifacts/HelloUDPTest.jar:./homework9/out/artifacts/homework9_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.hello.Tester client ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPClient "$1"
