#! /bin/bash
java -cp ./java-advanced-2016/artifacts/ImplementorTest.jar:./homework4/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.implementor.Tester jar-class ru.ifmo.ctddev.ohanjanyan.implementor.Implementor $1
