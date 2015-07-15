#!/bin/bash
cd random
if [ $(uname -m) = "x86_64" ]
then
	echo "Testing sampling library (static BFL)"
	echo "load_foreign_files(['sampling.static'],[],init_my_predicates). gaussian([1],[1],[Val]), write('sample from gaussian(1,1): '),writeln(Val),halt. halt(1)." | yap -q  2> errors.txt
	if [ $? -eq 0 ]
	then
	  cp sampling.static.so sampling.so
	  echo "Everything works!"
	  exit
	fi
	echo "Static library does not work, trying with dynamic BFL"
	echo "load_foreign_files(['sampling.sharedlocal'],[],init_my_predicates). gaussian([1],[1],[Val]), write('sample from gaussian(1,1): '),writeln(Val),halt. halt(1)." | yap -q  2>> errors.txt
	if [ $? -eq 0 ]
	then
	  cp sampling.sharedlocal.so sampling.so
	  echo "Everything works!"
	  exit
	fi

else
	echo "Testing sampling library (static BFL 32 bit)"
	echo "load_foreign_files(['sampling.static32'],[],init_my_predicates). gaussian([1],[1],[Val]), write('sample from gaussian(1,1): '),writeln(Val),halt. halt(1)." | yap -q  2> errors.txt
	if [ $? -eq 0 ]
	then
	  cp sampling.static32.so sampling.so
	  echo "Everything works!"
	  exit
	fi
fi

echo "Precompiled code does not work, you need to compile manually"
cat errors.txt
