#! /bin/bash
javadoc -classpath ../java-advanced-2016/artifacts/*:../java-advanced-2016/lib/* \
-link https://docs.oracle.com/javase/8/docs/api/ \
-sourcepath ../homework3/src:../homework6/src:../homeworkA/src:../java-advanced-2016/java \
ru.ifmo.ctddev.ohanjanyan.implementor info.kgeorgiy.java.advanced.implementor \
ru.ifmo.ctddev.ohanjanyan.concurrent info.kgeorgiy.java.advanced.concurrent \
ru.ifmo.ctddev.ohanjanyan.bank ru.ifmo.ctddev.ohanjanyan.bank.test
