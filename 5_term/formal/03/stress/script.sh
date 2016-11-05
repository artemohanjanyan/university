#!/bin/bash
for ((j=0;j<100;++j))
do
	echo $j
	s1=$(cat tests/$j | ./s1)
	s2=$(cat tests/$j | ./s2)
	if [ "$s1" != "$s2" ]
	then
		echo "WRONG"
		echo "WRONG"
		echo "WRONG"
	fi
done
