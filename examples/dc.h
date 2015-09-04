#include <iostream>
#include <exception>
#include <string>
#include <Yap/YapInterface.h>
#include <Yap/c_interface.h>

using namespace std;

#ifndef DC_H
#define DC_H

class initerrordc: public std::exception
{
	virtual const char* what() const throw()
	{
	return "Initialization error: file not found or particle initialization failed";
	}
};


class dc
{
	private:
		bool yaploaded=false;
		bool load(string file);
	protected:
		YAP_Term emptylist,yap_false,yap_true;
	public:
		dc(string file);
		~dc();
		double query(uint n, string query,string evidence);
		bool exec(string q);
		static void print(YAP_Term t) { YAP_Write(t,0,YAP_WRITE_HANDLE_VARS); };
		static void printnl(YAP_Term t) { YAP_Write(t,0,YAP_WRITE_HANDLE_VARS);cout<<endl; };
		bool	runGoalOnce(YAP_Term tmp,int argOutput, int &out);
		bool	runGoalOnce(YAP_Term tmp,int argOutput, double &out);
		YAP_Term runGoalOnce(YAP_Term tmp,int argOutput);
};
#endif
