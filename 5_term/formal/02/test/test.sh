#!/bin/bash
for ((i=0;;++i))
do
	input=$(java gen $i)
	correct=$(echo $input | ./correct)
	user=$(echo $input | ./probe)
	conclusion=$(echo $correct $user | ./check)

	echo $i
	if [ "$conclusion" == "YES" ]
	then
		echo "test passed"
	else
		echo "INCORRECT RESULT FOUND"
		echo "Correct:"
		echo "$correct"
		echo "User:"
		echo "$user"
		echo "Input:"
		echo "$input"
		echo
	fi
done
