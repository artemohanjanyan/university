#! /bin/bash
javac -cp ../java-advanced-2016/artifacts/ImplementorTest.jar ../homework3/src/ru/ifmo/ctddev/ohanjanyan/implementor/* -d .
echo "Class-Path: ../java-advanced-2016/artifacts/ImplementorTest.jar" > classpath
jar emcf ru.ifmo.ctddev.ohanjanyan.implementor.Main classpath Implementor.jar ru/ifmo/ctddev/ohanjanyan/implementor/*
rm -r ru classpath
