#include <iostream>
#include <string>
#include "dcpf.h"

int main()
{
	dcpf filter("dcpfwumpus.pl",1000); // initialization particle filter loading the model and setting 1000 particles
	bool res;
	res=filter.step("action(null)","observation(up) ~= wall,observation(right) ~= free",1); // perform 1 step (propagation + weighting + resampling)
	cout<<res<<endl;
	res=filter.exec("plotdata(1)");
	cout<<res<<endl;
	res=filter.exec("printp(1)"); // print the content of the first particle
	cout<<res<<endl;
	double p=filter.query("maze(0,1):t~=wall");
	cout<<"probability maze(0,1):t"<<endl<< p<<endl;
	filter.exec("time(test1(1000))");
	return 0;
}
/*

compile using (the lib path /usr/local/lib can be different):
g++ -o main.o main.cpp dcpf.cpp -lYap -L/usr/local/lib  -std=c++0x

execute:
./main.o

*/
