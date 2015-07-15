#include <iostream>
#include <exception>
#include <string>
#include <Yap/YapInterface.h>
#include <Yap/c_interface.h>

using namespace std;

#ifndef DCPF_H
#define DCPF_H

class initerrordcpf: public std::exception
{
	virtual const char* what() const throw()
	{
	return "Initialization error: file not found or particle initialization failed";
	}
};


class dcpf
{
	private:
		int n;
	public:
		dcpf(char *file, int numParticles);
		bool step(string actions,string observations,double delta);
		double query(string q);
		bool exec(string q);
};
#endif
