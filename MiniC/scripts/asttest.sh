#!/bin/bash

tst=./Parser/tst/base/AST_testcases
sol=./Parser/tst/base/AST_solutions_trees
ans=./Parser/results
report=$ans/report.txt
all=0
ok=0

rm -rf $ans
mkdir -p $ans
echo "Parser Test Report" >$report
echo "generated "`date` >>$report
echo "Testing the parser..."
for file in $tst/c*.mc
do
     all=$(( $all + 1 ))
     f=`basename $file`
     java -ea MiniC.MiniC -t $ans/s_$f $file > /dev/null 2>&1
     diff -u --ignore-all-space --ignore-blank-lines $ans/s_$f $sol/${f}.ast > $ans/diff_$f 2>&1
     if [ "$?" -ne 0 ]
     then
		 echo -n "-"
                 echo "$f failed" >> $report

     else
                 echo -n "+"
                 echo "$f succeded" >> $report
                 rm -rf $ans/diff_$f $ans/s_$f
                 ok=$(( $ok + 1 ))
     fi
done
echo
echo "Testing finished, pls. consult the test report in $ans."
echo "$ok out of $all testcases succeeded."
