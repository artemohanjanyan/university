#!/bin/bash
export CLASSPATH=out/production/homeworkA

java -cp $CLASSPATH Client "$@"
