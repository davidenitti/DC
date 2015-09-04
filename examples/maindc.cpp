#include <iostream>
#include <string>
#include "dc.h"

int main()
{
	dc d("example1.pl"); // initialization particle filter loading the model and setting 1000 particles
	std::cout<<d.query(1000,"drawn(1) ~= 2","")<<endl;
	return 0;
}
/*

compile using (the lib path /usr/local/lib can be different):
g++ -o dc maindc.cpp dc.cpp -lYap -L/usr/local/lib  -std=c++0x

execute:
./main.o

*/
