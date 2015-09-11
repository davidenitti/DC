#include <iostream>
#include <string>
#include "planner.h"

using namespace std;
int main()
{
	planner d("../../planning/blockworld2.4_new.pl",false); // initialization particle filter loading the model and setting 1000 particles
	d.exec("getparam('bw')");
	YAP_Term a =d.getBestAction2("observation(list([1,2,3,4,table]))~= true,observation(on(3,2))~= true,observation(on(2,1))~= true,observation(on(1,table))~= true,observation(on(4,table))~= true,observation(clear(4))~= true,observation(clear(3))~= true,observation(clear(table))~= true",
								10,10);
/*
	char buffer[100];
	YAP_WriteBuffer(a,buffer,100,0);
	cout<<"best action "<<buffer<<endl;*/
	dc::printnl(a);
	return 0;
}
/*

compile using (the lib path /usr/local/lib can be different):
g++ -o dc maindc.cpp dc.cpp -lYap -L/usr/local/lib  -std=c++0x

execute:
./main.o

*/
