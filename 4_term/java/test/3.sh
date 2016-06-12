#! /bin/bash
java -cp ./java-advanced-2016/artifacts/ImplementorTest.jar:./homework4/Implementor.jar:./java-advanced-2016/lib/junit-4.11.jar:./java-advanced-2016/lib/hamcrest-core-1.3.jar \
	info.kgeorgiy.java.advanced.implementor.Tester class ru.ifmo.ctddev.ohanjanyan.implementor.Implementor $1
