#!/bin/bash
cd random
make clean
make static 2> errors.txt
if [ $? -eq 0 ]
then
	echo "Testing sampling library (static BFL)"
		echo "load_foreign_files(['sampling'],[],init_my_predicates). gaussian([1],[1],[Val]), write('sample from gaussian(1,1): '),writeln(Val),halt. halt(1)." | yap -q  2>> errors.txt
		if [ $? -eq 0 ]
		then
		  echo "Compilation complete!"
		  exit
		fi
fi	
echo "Compilation failed, you need to compile manually"
cat errors.txt
