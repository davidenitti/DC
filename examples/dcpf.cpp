#include "dcpf.h"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>

initerrordcpf exceptiondcpf;
dcpf::dcpf(char *file, int numParticles):n(numParticles)
{
	YAP_Term error;
	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		throw exceptiondcpf;

	exec("yap_flag(informational_messages,off)");
	string goal="consult('";
	goal+=file+string("')");

	int res = exec(goal);
	if(res!=1)
		throw exceptiondcpf;

	char temp[100];
	sprintf(temp,"init_particle(%d)",numParticles);
	res=YAP_RunGoalOnce(YAP_ReadBuffer(temp,&error));
	if(res!=1)
		throw exceptiondcpf;
}

bool dcpf::step(string actions,string observations,double delta=1.0)
{
	string stepstring="step_particle([" + actions + "],[" + observations + "]," + std::to_string(n) + "," + std::to_string(delta) +")";
//	cout<<stepstring<<endl;
	YAP_Term error;
	char command[1000];
	strcpy(command,stepstring.c_str());
	int res = YAP_RunGoalOnce(YAP_ReadBuffer(command,&error));
	return res;
}
// evaluate probability of a query: eval_query_particle(Query,Nparticles,Probability), where Query=query
double dcpf::query(string query)
{
	YAP_Term error;
	string goal="eval_query_particle(" + query + "," + std::to_string(n) + ",ProbabilityQuery)";
//	cout<<goal;
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return -1;
	double prob=YAP_FloatOfTerm( YAP_ArgOfTerm(3,YAP_GetFromSlot(safe_t)) );
	YAP_RecoverSlots(1); // safe copy not needed anymore
	return prob;
}

// execute an arbitrary prolog goal (query)
bool dcpf::exec(string q)
{
	YAP_Term error;
	int res = YAP_RunGoalOnce(YAP_ReadBuffer(q.c_str(),&error));
	return res;
}
