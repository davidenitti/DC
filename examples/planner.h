#include <iostream>
#include <exception>
#include <string>
#include <vector>
#include <utility>
#include <Yap/YapInterface.h>
#include <Yap/c_interface.h>
#include "dc.h"
using namespace std;

#ifndef PLANNER_H
#define PLANNER_H

// planner in DC
class planner: public dc
{
	private:
		bool abstraction;
		Functor simpleplan,executedplan_step;
	public:
		planner(char *file,bool abstract):dc(file),abstraction(abstract)
		{
			simpleplan=YAP_MkFunctor(YAP_LookupAtom("simpleplan"),9);
			executedplan_step=YAP_MkFunctor(YAP_LookupAtom("executedplan_step"),9);
			exec("executedplan_start");
		};
//		vector<pair<YAP_Term,double>>getQ(string currentstate);
		YAP_Term getBestAction(string currentstate,uint samples,uint depth);
		YAP_Term getBestAction2(string currentstate,int samples,int depth);
		YAP_Term getBestAction3(string currentstate,uint samples,uint depth,uint depthsearch);
		bool executeAction(string currentstate);
};
#endif
