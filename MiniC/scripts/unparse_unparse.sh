#!/bin/bash

tst=./Parser/tst/base/AST_testcases
ans=./Parser/results
report=$ans/report.txt
all=0
ok=0

rm -rf $ans
mkdir -p $ans
echo "Parser Test Report" >$report
echo "generated "`date` >>$report
#
# Run testcases:
#
echo "Testing the parser..."
for file in $tst/c*.mc
do
     all=$(( $all + 1 ))
     f=`basename $file`
     java -ea MiniC.MiniC -u $ans/u_$f $file > /dev/null 2>&1
     java -ea MiniC.MiniC -u $ans/uu_$f $ans/u_$f > /dev/null 2>&1
     diff -u --ignore-all-space --ignore-blank-lines $ans/u_$f $ans/uu_${f} > $ans/diff_$f 2>&1
     if [ "$?" -ne 0 ]
     then
		 echo -n "-"
                 echo "$f failed" >> $report

     else
                 echo -n "+"
                 echo "$f succeded" >> $report
                 rm -rf $ans/diff_$f $ans/u_$f $ans/uu_$f
                 ok=$(( $ok + 1 ))
     fi
done
echo
echo "Testing finished, pls. consult the test report in $ans."
echo "$ok out of $all testcases succeeded."
