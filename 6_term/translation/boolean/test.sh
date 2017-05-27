#!/bin/bash
make

declare -a tests=('(a and b) or not (c xor (a or not b))'
                  '((((((a and b) or not (c xor (a or not b)))))))'
                  '((((((a and b) or not (c xor (a or not b))))))'
                  '(((((a and b) or not (c xor (a or not b)))))))'
                  'a and b and c'
                  'a and b or c'
                  'a or b and c'
                  'a or b or c'
                  'a impl b or c impl d or e and not f'
                  'a impl b impl c'
                  '')

for ((i = 0; i < ${#tests[@]}; i++))
do
   test="${tests[$i]}"
   echo "TEST $test"
   if echo $test | ./main $i
   then
      dot -Tsvg $i -o $i.svg
      rm $i
   else
      echo "FAIL"
   fi
   echo
   echo "=========="
   echo
   echo
done
