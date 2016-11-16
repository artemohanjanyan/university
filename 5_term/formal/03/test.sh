#!/bin/bash
for i in ${1}tests/*.in
do
	echo $i
	correct=$(cat ${i:0:-2}out)
	user=$(cat $i | ./${1})

	if [ "$correct" != "$user" ]
	then
		echo "INCORRECT!"
		echo "Correct:"
		echo "$correct"
		echo "User:"
		echo "$user"
		echo
	else
		echo "Passed"
		echo
	fi
done
