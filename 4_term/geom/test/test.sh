#!/bin/bash
for ((i=0;i<1000;++i))
do
	echo $i
	python gen.py
	mv segments.in "segments$i{}.in"
	s1=$(cat segments$i{}.in | ./segments)
	s2=$(cat segments$i{}.in | ./F)
	if [ "$s1" != "$s2" ]
	then
		echo "WRONG"
		echo "WRONG"
		echo "WRONG"
	else
		rm "segments$i{}.in"
	fi
	sleep 1
done
