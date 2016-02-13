#include "dc.h"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>

initerrordc exceptiondc;
dc::dc(string file)
{
	if(load(file)!=1)
		throw exceptiondc;
	YAP_Term error;
	emptylist=YAP_ReadBuffer("[]",&error);
	yap_false=YAP_ReadBuffer("false",&error);
	yap_true=YAP_ReadBuffer("true",&error);
}
dc::~dc()
{
	//YAP_Exit(0);
}
bool dc::load(string file)
{
//	if(yaploaded)
//		YAP_Exit(0);
	YAP_Term error;
	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		return false; //throw exceptiondc;
	else
		yaploaded=true;
	exec("yap_flag(informational_messages,off)");
	string goal="consult('";
	goal+=file+string("')");

	return exec(goal); // 1 ok
}



// evaluate probability of a query: query([evidence],[],query,n,P,_,_)
double dc::query(uint n, string query,string evidence)
{
	YAP_Term error;
/*	YAP_Atom q=YAP_LookupAtom("query");
	cout<<YAP_AtomName(q);
*/
	string goal="query([" + evidence + "],[],"+query + "," + std::to_string(n) + ",ProbabilityQuery,_,_)";
//	cout<<goal;
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	double prob;
	if(!runGoalOnce(tmp,5,prob))
		return -1;
	return prob;
}

// execute an arbitrary prolog goal (query)
bool dc::exec(string q)
{
	YAP_Term error;
	int res = YAP_RunGoalOnce(YAP_ReadBuffer(q.c_str(),&error));
	return res;
}
bool dc::runGoalOnce(YAP_Term tmp,int argOutput, int &out)
{
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return false;
	out = YAP_IntOfTerm(YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t)));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	return true;
}
bool dc::runGoalOnce(YAP_Term tmp,int argOutput, double &out)
{
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return false;
	out = YAP_FloatOfTerm(YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t)));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	return true;
}
YAP_Term dc::runGoalOnce(YAP_Term tmp,int argOutput)
{
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return 0;
	YAP_Term out = YAP_FloatOfTerm(YAP_ArgOfTerm(argOutput,YAP_GetFromSlot(safe_t)));
	YAP_RecoverSlots(1); // safe copy not needed anymore
	return out;
}
