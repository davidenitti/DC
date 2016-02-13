#include "planner.h"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
YAP_Term planner::getBestAction3(string currentstate,uint samples,uint depth,uint depthsearch)
{
	YAP_Term error;
// executedplan_step(BAction,Abstract,Init,N,MaxD,TotalR,T,MaxDSearch,STOP)
	string goal="executedplan_step(BAction,"+ std::to_string(abstraction)+
	",["+currentstate+"],"+ std::to_string(samples) +"," + std::to_string(depth) +
	",TotalR,T," + std::to_string(depthsearch)+ "STOP)";
//	cout<<goal;
//	YAP_Exit(0);
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	//YAP_handle_t
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return -1;

	YAP_Term bestAction = YAP_ArgOfTerm(9,YAP_GetFromSlot(safe_t)) ;

	YAP_RecoverSlots(1); // safe copy not needed anymore
	return bestAction;
}

YAP_Term planner::getBestAction(string currentstate,uint samples,uint depth)
{
	YAP_Term error;
	string goal="simpleplan(0,"+ std::to_string(abstraction)+
	",["+currentstate+"],[],"+ std::to_string(samples) +"," + std::to_string(depth)+",Avg,FinalT,BAction)";
//	cout<<goal;
//	YAP_Exit(0);
	YAP_Term tmp = YAP_ReadBuffer(goal.c_str(),&error);
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return -1;

	YAP_Term bestAction = YAP_ArgOfTerm(9,YAP_GetFromSlot(safe_t)) ;

	YAP_RecoverSlots(1); // safe copy not needed anymore
	return bestAction;
}

YAP_Term planner::getBestAction2(string currentstate,int samples,int depth)
{
	YAP_Term error;
	YAP_Term arg[9];
	arg[0]=YAP_MkIntTerm(0);
	arg[1]=(abstraction ? yap_true : yap_false);
	arg[2]=YAP_ReadBuffer((string("[")+currentstate+string("]")).c_str(),&error);
	arg[3]=emptylist;//YAP_ReadBuffer("[]",&error);
	arg[4]=YAP_MkIntTerm(samples);
	arg[5]=YAP_MkIntTerm(depth);
	arg[6]=YAP_MkVarTerm();
	arg[7]=YAP_MkVarTerm();
	arg[8]=YAP_MkVarTerm();
//	YAP_Exit(0);
	YAP_Term tmp = YAP_MkApplTerm( simpleplan , 9, arg);
	printnl(tmp);
	long safe_t = YAP_InitSlot(tmp); // have a safe pointer to term
	int res = YAP_RunGoalOnce(tmp);
	if (res==false)
		return -1;

	YAP_Term bestAction = YAP_ArgOfTerm(9,YAP_GetFromSlot(safe_t)) ;

	YAP_RecoverSlots(1); // safe copy not needed anymore
	return bestAction;
}
