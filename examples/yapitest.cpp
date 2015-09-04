#include <iostream>
#include <string>
#include "yapi.hh"

int main()
{
	int i=1;
	YAPEngine y;
	YAPQuery query("true.");
	std::cout<<"ok"<<std::endl;
	return 0;
}

/*

g++ -g -o y yapitest.cpp /home/davide/Desktop/dtai/vcosta/yap-6.3/CXX/yapi.cpp  -lYap -L/usr/local/lib -I/usr/local/include/Yap/src -I/home/davide/Desktop/dtai/vcosta/yap-6.3/CXX -I/usr/local/include/Yap -I/home/davide/Desktop/dtai/vcosta/yap-6.3/H -I/home/davide/Desktop/dtai/vcosta/yap-6.3/os -std=c++0x
execute:
./y

*/
