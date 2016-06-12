#! /bin/bash
javadoc -classpath ../java-advanced-2016/artifacts/ImplementorTest.jar:../java-advanced-2016/lib/junit-4.11.jar \
-link https://docs.oracle.com/javase/8/docs/api/ \
-sourcepath ../homework3/src:../java-advanced-2016/java \
-private ru.ifmo.ctddev.ohanjanyan.implementor info.kgeorgiy.java.advanced.implementor
