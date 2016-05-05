for ((i=0;i<448;++i))
do
	echo $i
	s1=$(echo "4 $i" | ./gen)
	echo " $s1"
	s2=$(echo $s1 | ./a.out)
	echo " $s2"
	if [ "$i" != "$s2" ]
	then
		echo "WRONG"
		echo "WRONG"
		echo "WRONG"
	fi
done
