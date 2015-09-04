// Copyright 2014, Davide Nitti, KU Leuven. All rights reserved.
#include <Yap/YapInterface.h>
#include <unistd.h>

#include <iostream>
#include <fstream>
#include <map>

#include <bfl/bfl_err.h>
#include <bfl/wrappers/matrix/vector_wrapper.h>
#include <bfl/wrappers/matrix/matrix_wrapper.h>
#include <bfl/pdf/gaussian.h>

#include <bfl/filter/extendedkalmanfilter.h>

#include <bfl/model/linearanalyticsystemmodel_gaussianuncertainty.h>
#include <bfl/model/linearanalyticmeasurementmodel_gaussianuncertainty.h>

#include <bfl/pdf/analyticconditionalgaussian.h>
#include <bfl/pdf/linearanalyticconditionalgaussian.h>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf_gamma.h>

#include <math.h> 

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>

#include <ctime>
#include <random>
extern boost::mt19937 Boost_Rng;
//#include "rng.h"

/*

export LD_LIBRARY_PATH=/usr/local/lib/:/usr/lib/
export LD_INCLUDE_PATH=/usr/include/



*/

using namespace MatrixWrapper;
using namespace BFL;
using namespace std;
  
gsl_rng *r;
default_random_engine generator;

static void initrand()
{
	generator=default_random_engine((unsigned int)time(0));
	const gsl_rng_type * T;   // setup RNG
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc(T);
	gsl_rng_set(r, time (NULL) * getpid());
	Boost_Rng.seed(time (NULL) * getpid()); // seed is the time in seconds, every second the seed changes.
	/*
	#include <sys/time.h>

	struct timeval now;
	gettimeofday(&now, NULL);
	Boost_Rng.seed(now.tv_usec);
	cout<<now.tv_usec;
	 */
}

void do_putc(int c)
{
	printf("%c",c);
}

static bool pl_test(void)
{
/*
	int sum=0;
	int dimension=0;
  	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		sum+= YAP_IntOfTerm(head);
		dimension++;
	}

  YAP_Term out = YAP_ARG2;

  YAP_Term val= YAP_MkIntTerm(dimension);
  return(YAP_Unify(out,val));
  */
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	
	ColumnVector mean(dimension);
	SymmetricMatrix covariance(dimension);
	
	list = YAP_ARG1;
	
	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	/*
	YAP_Term covMatrix = YAP_ARG2;
	for(int a=0;a<dimension;a++)
	{
		for(int b=0;b<dimension;b++)
		{
			head = YAP_HeadOfTerm(covMatrix);
			
			if(YAP_IsIntTerm(head))
				covariance(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				covariance(a,b) = YAP_FloatOfTerm(head);
			else
				return false;
				
			list = YAP_TailOfTerm(covMatrix);
		}
	}
	
			
	//Gaussian gauss(mean, covariance);
	//Sample<ColumnVector> sample(dimension);
	//gauss.SampleFrom(sample,DEFAULT,NULL);
*/
	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(mean[dimension-1]),YAP_MkAtomTerm(YAP_LookupAtom("[]")));
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(mean[a]), val);
	}
	YAP_Term out = YAP_ARG2;
	return(YAP_Unify(out,val)); // testsum([1,2,3],A).
}

static bool pl_initmap(void)
{
	map<YAP_Term, double> *weights=new map<YAP_Term, double>;
	return(YAP_Unify(YAP_ARG1,YAP_MkIntTerm((long)weights)));
}
static bool pl_addvaluemap(void)
{
	map<YAP_Term, double> *weights=(map<YAP_Term, double>*) YAP_IntOfTerm(YAP_ARG1);
	double w;
	if(YAP_IsIntTerm(YAP_ARG3))
		w = YAP_IntOfTerm(YAP_ARG3);
	else if(YAP_IsFloatTerm(YAP_ARG3))
		w = YAP_FloatOfTerm(YAP_ARG3);
	else
		return false;
	if(weights->count(YAP_ARG2)==0)
		(*weights)[YAP_ARG2]=w;
	else
		(*weights)[YAP_ARG2]+=w;
//	cout<<(*weights)[YAP_ARG2];
}

static bool pl_averagemap(void)
{
	map<YAP_Term, double> *weights=(map<YAP_Term, double>*) YAP_IntOfTerm(YAP_ARG1);

	double totalw=0.0;
	double average=0.0;
	for (auto& kv : *weights)
	{
		double val;
		cout << kv.first;
		if(YAP_IsIntTerm(kv.first))
			val = YAP_IntOfTerm(kv.first);
		else if(YAP_IsFloatTerm(kv.first))
			val = YAP_FloatOfTerm(kv.first);
		else
			return false;
		cout << val << " has value " << kv.second << std::endl;
		totalw+=kv.second;
		average+=val*kv.second;
	}
	average/=totalw;
	return(YAP_Unify(YAP_ARG2,YAP_MkFloatTerm(average) ));
}

static bool pl_deletemap(void)
{
	map<YAP_Term, double> *weights=(map<YAP_Term, double>*) YAP_IntOfTerm(YAP_ARG1);
	delete weights;
}

static bool pl_discreteuniform(void) // https://www.gnu.org/software/gsl/manual/html_node/Sampling-from-a-random-number-generator.html
{
	int n;

	if(YAP_IsIntTerm(YAP_ARG1))
		n = YAP_IntOfTerm(YAP_ARG1);
	else
		return false;

	int sampled = gsl_rng_uniform_int(r,n);
	YAP_Term val = YAP_MkIntTerm(sampled);
	return(YAP_Unify(YAP_ARG2,val));
}

bool fillVector(YAP_Term list,ColumnVector &v)
{
	int i=0;
	YAP_Term head;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);

		if(YAP_IsIntTerm(head))
			v[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			v[i] = YAP_FloatOfTerm(head);
		else
			return false;

		list = YAP_TailOfTerm(list);
		i++;
	}
	return true;
}
bool fillMatrix(YAP_Term t,int row,int c,SymmetricMatrix &m)
{
	YAP_Term matrix = t,head;
	for(int a=1;a<=row;a++)
	{
		for(int b=1;b<=c;b++)
		{
			head = YAP_HeadOfTerm(matrix);

			if(YAP_IsIntTerm(head))
				m(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				m(a,b) = YAP_FloatOfTerm(head);
			else
				return false;

			matrix = YAP_TailOfTerm(matrix);
		}
	}
	return true;
}
bool fillMatrix(YAP_Term t,int row,int c,Matrix &m)
{
	YAP_Term matrix = t,head;
	for(int a=1;a<=row;a++)
	{
		for(int b=1;b<=c;b++)
		{
			head = YAP_HeadOfTerm(matrix);

			if(YAP_IsIntTerm(head))
				m(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				m(a,b) = YAP_FloatOfTerm(head);
			else
				return false;

			matrix = YAP_TailOfTerm(matrix);
		}
	}
	return true;
}

// optimal proposal distribution  for Nonlinear Gaussian State Space Models (paper On Sequential Monte Carlo Sampling Methods for Bayesian Filtering, pag 199)
// test optimalproposal([1,0],[0.1,0,0,0.1],[1,0],[0.01],[1.1],A,B).
// args: 1: f(x_{t-1}), 2: sigma_v, 3: C, 4: sigma_w, 5: y, 6: sampled state, 7: weight
static bool pl_proposalDistribution(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension<1)
		return 0;
	cout<<dimension;
	ColumnVector fOfX(dimension); // f(x_{t-1})
	SymmetricMatrix covSTM(dimension); // sigma_v

	fillVector(YAP_ARG1,fOfX);
	cout<<"fOfX "<<fOfX<<endl;
	fillMatrix(YAP_ARG2,dimension,dimension,covSTM);
	Matrix sigmaV(covSTM);
	cout<<"sigmaV "<<sigmaV<<endl;

	int output_dim=0;
	list = YAP_ARG5;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		output_dim++;
	}

	ColumnVector y(output_dim); // y_t
	fillVector(YAP_ARG5,y);
	cout<<"y "<<y<<endl;
	Matrix C(output_dim,dimension);
	fillMatrix(YAP_ARG3,output_dim,dimension,C);
	cout<<"C "<<C<<endl;
	SymmetricMatrix covMM(output_dim); // sigma_w
	fillMatrix(YAP_ARG4,output_dim,output_dim,covMM);
	cout<<"sigma_v "<<covSTM<<endl;
	cout<<"sigma_w "<<covMM<<endl;
	Matrix sigmaW(covMM);
	Matrix covProposalInverse(dimension,dimension);
	Matrix covSTMInverse(SymmetricMatrix(covSTM.inverse()));

	cout<<"covSTMInverse "<<covSTMInverse<<endl;
	Matrix covMMInverse(SymmetricMatrix(covMM.inverse()));
	cout<<"sigma_w inv "<<covMMInverse<<endl;
	covProposalInverse = covSTMInverse+C.transpose()*covMMInverse*C;
	cout<<"covProposalInverse "<<covProposalInverse<<endl;
	SymmetricMatrix covProposal(covProposalInverse.inverse());
	Matrix I(dimension,dimension);
	for(int i=1;i<=dimension;i++)
		for(int j=1;j<=dimension;j++)
			I(i,j)=0;
	for(int i=1;i<=dimension;i++)
		I(i,i)=1;

	cout<<"covProposal "<<covProposal<<endl;
	covProposal=SymmetricMatrix((I + covSTM * (C.transpose() * covMMInverse * C) ).inverse() * Matrix(covSTM));
	cout<<"covProposal "<<covProposal<<endl;
	Matrix temp=(I + covSTM * (C.transpose() * covMMInverse * C) ).inverse();
	SymmetricMatrix temp2=SymmetricMatrix( temp*Matrix(covSTM) );
	cout<<"I "<<I<<endl;
	cout<<"temp2 "<<temp2<<endl;

	ColumnVector bb=( covSTMInverse * fOfX + (C.transpose() * covMMInverse * y) );
	Matrix A=(covProposalInverse);
	cout<<"A "<<A<<endl;
	cout<<"b "<<bb<<endl;
	double *a_data = new double[A.rows()*A.rows()];
	double *b_data = new double[A.rows()];

	int i=0;
	for(int a=1;a<=A.rows();a++)
	{
		for(int b=1;b<=A.rows();b++)
		{
			a_data[i++]=A(a,b);
			//cout<<A(a,b)<<endl;
		}
		b_data[a-1]=bb[a-1];
		cout<<"b_data[a-1]"<<b_data[a-1]<<endl;
	}

	gsl_matrix_view m = gsl_matrix_view_array (a_data, A.rows(), A.rows());
	gsl_vector_view b = gsl_vector_view_array (b_data, A.rows());
	gsl_vector *x = gsl_vector_alloc (A.rows());

	int s;
	gsl_permutation * p = gsl_permutation_alloc (A.rows());
	gsl_linalg_LU_decomp (&m.matrix, p, &s);
	gsl_linalg_LU_solve (&m.matrix, p, &b.vector, x);


	ColumnVector meanProposal(dimension);
	meanProposal = covProposal * (covSTMInverse * fOfX + (C.transpose() * covMMInverse * y) );
	cout<<"meanProposal "<<meanProposal<<endl;

	for(int i=0;i<A.rows();i++)
		meanProposal[i]=gsl_vector_get(x,i);

	printf ("x = \n");
	gsl_vector_fprintf (stdout, x, "%g");
	gsl_permutation_free (p);
	gsl_vector_free (x);
	cout<<"meanProposal "<<meanProposal<<endl;

	ColumnVector meanW(C * fOfX);

	SymmetricMatrix covW( sigmaW + C * sigmaV * (C.transpose())  );

	Gaussian gaussW(meanW, covW);
	cout<<"meanW "<<meanW<<endl;
	cout<<"covW "<<covW<<endl;
	double weight = gaussW.ProbabilityGet(y).getValue();

	/*
	Matrix firstTerm(1,(y-C*fOfX).transpose());
	cout<<"firstTerm "<<firstTerm<<endl;
	Matrix secondTerm((sigmaV + C * sigmaW * (C.transpose()) ).inverse());

	ColumnVector thirdTerm((y - C * fOfX));
	cout<<"secondTerm "<<secondTerm<<endl;
	cout<<"thirdTerm "<<thirdTerm<<endl;
	//cout<<( firstTerm * secondTerm * (y - C * fOfX) ).rows()<<endl;

	Matrix A(sigmaV + C * sigmaW * (C.transpose()) );
	cout<<"A "<<A<<endl;

	double *a_data = new double[A.rows()*A.rows()];
	double *b_data = new double[A.rows()];
	int i=0;
	for(int a=0;a<A.rows();a++)
	{
		for(int b=0;b<A.rows();b++)
		{
			a_data[i++]=A[a][b];
		}
		b_data[a]=thirdTerm[a];
	}
	return 0;
	gsl_matrix_view m = gsl_matrix_view_array (a_data, A.rows(), A.rows());

	gsl_vector_view b = gsl_vector_view_array (b_data, A.rows());

	gsl_vector *x = gsl_vector_alloc (A.rows());

	int s;

	gsl_permutation * p = gsl_permutation_alloc (A.rows());

	gsl_linalg_LU_decomp (&m.matrix, p, &s);

	gsl_linalg_LU_solve (&m.matrix, p, &b.vector, x);

	printf ("x = \n");
	gsl_vector_fprintf (stdout, x, "%g");

	gsl_permutation_free (p);
	gsl_vector_free (x);
	cout<<"(secondTerm * thirdTerm) "<<(secondTerm * thirdTerm)<<endl;

	double weight=exp(-0.5 * ( firstTerm * secondTerm * thirdTerm )[0]  );
	*/
	Gaussian gauss(meanProposal, covProposal);
	Sample<ColumnVector> sample(dimension);
	cout<<"weight "<<weight<<endl;
	gauss.SampleFrom(sample,DEFAULT,NULL);
	cout<<"sample "<<sample<<endl;
	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(sample.ValueGet()[dimension-1]), YAP_MkAtomTerm(YAP_LookupAtom("[]")) );
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(sample.ValueGet()[a]), val);
	}
	delete [] a_data;
	delete [] b_data;
	YAP_Term weightTerm = YAP_MkFloatTerm(weight);
	return(YAP_Unify(YAP_ARG6,val) && YAP_Unify(YAP_ARG7,weightTerm));
}

static bool pl_matrixproduct(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension<1)
		return 0;
//	cout<<dimension<<endl;
	ColumnVector Axt(dimension); // Ax_t
	ColumnVector xnext(dimension); // x_{t+1}
	SymmetricMatrix cov(dimension); // sigma_v

	fillVector(YAP_ARG1,Axt);
	fillVector(YAP_ARG2,xnext);
//	cout<<"Axt "<<Axt<<endl;
	fillMatrix(YAP_ARG3,dimension,dimension,cov);

//	cout<<"cov "<<cov<<endl;

	ColumnVector bb=xnext-Axt;
	Matrix A(cov);
//	cout<<"A "<<A<<endl;
//	cout<<"b "<<bb<<endl;
	double *a_data = new double[A.rows()*A.rows()];
	double *b_data = new double[A.rows()];

	int i=0;
	for(int a=1;a<=A.rows();a++)
	{
		for(int b=1;b<=A.rows();b++)
		{
			a_data[i++]=A(a,b);
//			cout<<A(a,b)<<endl;
		}
		b_data[a-1]=bb[a-1];
//		cout<<"b_data[a-1]"<<b_data[a-1]<<endl;
	}

	gsl_matrix_view m = gsl_matrix_view_array (a_data, A.rows(), A.rows());
	gsl_vector_view b = gsl_vector_view_array (b_data, A.rows());
	gsl_vector *x = gsl_vector_alloc (A.rows());

	int s;
	gsl_permutation * p = gsl_permutation_alloc (A.rows());
	gsl_linalg_LU_decomp (&m.matrix, p, &s);
	gsl_linalg_LU_solve (&m.matrix, p, &b.vector, x);


	ColumnVector res(dimension);

	for(int i=0;i<A.rows();i++)
		res[i]=gsl_vector_get(x,i);

	double output=( bb.transpose() * res );
	delete [] a_data;
	delete [] b_data;
	YAP_Term outputTerm = YAP_MkFloatTerm(output);
	return(YAP_Unify(YAP_ARG4,outputTerm));
}

static bool pl_multivariateGaussian(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	
	ColumnVector mean(dimension);
	SymmetricMatrix covariance(dimension);
	
	list = YAP_ARG1;
	
	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	
	YAP_Term covMatrix = YAP_ARG2;
	for(int a=1;a<=dimension;a++)
	{
		for(int b=1;b<=dimension;b++)
		{
			head = YAP_HeadOfTerm(covMatrix);
			
			if(YAP_IsIntTerm(head))
				covariance(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				covariance(a,b) = YAP_FloatOfTerm(head);
			else
				return false;
				
			covMatrix = YAP_TailOfTerm(covMatrix);
		}
	}
	
			
	Gaussian gauss(mean, covariance);
	Sample<ColumnVector> sample(dimension);
	gauss.SampleFrom(sample,DEFAULT,NULL);

	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(sample.ValueGet()[dimension-1]), YAP_MkAtomTerm(YAP_LookupAtom("[]")) );
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(sample.ValueGet()[a]), val);
	}
	
	YAP_Term out = YAP_ARG3;
	return(YAP_Unify(out,val));
}

// NOT COMPLETE!
static bool pl_multivariateGaussian_cut(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}

	ColumnVector mean(dimension);
	SymmetricMatrix covariance(dimension);

	list = YAP_ARG1;

	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);

		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;

		list = YAP_TailOfTerm(list);
		i++;
	}

	YAP_Term covMatrix = YAP_ARG2;
	for(int a=1;a<=dimension;a++)
	{
		for(int b=1;b<=dimension;b++)
		{
			head = YAP_HeadOfTerm(covMatrix);

			if(YAP_IsIntTerm(head))
				covariance(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				covariance(a,b) = YAP_FloatOfTerm(head);
			else
				return false;

			covMatrix = YAP_TailOfTerm(covMatrix);
		}
	}

	YAP_Term linelist = YAP_ARG3;
	bool minmax = YAP_IntOfTerm( YAP_ARG4);
	ColumnVector line(dimension+1);
	i=0;
	while(YAP_IsPairTerm(linelist))
	{
		head = YAP_HeadOfTerm(linelist);

		if(YAP_IsIntTerm(head))
			line[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			line[i] = YAP_FloatOfTerm(head);
		else
			return false;

		linelist = YAP_TailOfTerm(linelist);
		i++;
	}

	Gaussian gauss(mean, covariance);
	Sample<ColumnVector> sample(dimension);
	gauss.SampleFrom(sample,DEFAULT,NULL);

	double limit=0;
	for(int a=0;a<dimension;a++)
	{
		limit += sample.ValueGet()[a] * line[a];
	}
	if(minmax)
	{
		if(limit>line[dimension])
			sample.ValueGet();// NOT COMPLETE!
	}

	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(sample.ValueGet()[dimension-1]), YAP_MkAtomTerm(YAP_LookupAtom("[]")) );
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(sample.ValueGet()[a]), val);
	}

	YAP_Term out = YAP_ARG5;
	return(YAP_Unify(out,val));
}

static bool pl_gamma(void) //alpha, beta, sampled value http://www.gnu.org/software/gsl/manual/html_node/The-Gamma-Distribution.html
{
	double alpha;

	if(YAP_IsIntTerm(YAP_ARG1))
		alpha = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		alpha = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	double beta;

	if(YAP_IsIntTerm(YAP_ARG2))
		beta = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		beta = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;

	double sampled = gsl_ran_gamma(r,alpha,beta);
	YAP_Term val = YAP_MkFloatTerm(sampled);
	return(YAP_Unify(YAP_ARG3,val));
}
static bool pl_gamma_pdf(void) //alpha, beta, sampled value http://www.gnu.org/software/gsl/manual/html_node/The-Gamma-Distribution.html
{
	double alpha;

	if(YAP_IsIntTerm(YAP_ARG1))
		alpha = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		alpha = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	double beta;

	if(YAP_IsIntTerm(YAP_ARG2))
		beta = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		beta = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;
	double x;

	if(YAP_IsIntTerm(YAP_ARG3))
		x = YAP_IntOfTerm(YAP_ARG3);
	else if(YAP_IsFloatTerm(YAP_ARG3))
		x = YAP_FloatOfTerm(YAP_ARG3);
	else
		return false;
	double pdf = gsl_ran_gamma_pdf(x,alpha,beta);
	YAP_Term val = YAP_MkFloatTerm(pdf);
	return(YAP_Unify(YAP_ARG4,val));
}


static bool pl_betafunction(void) //alpha, beta, http://www.gnu.org/software/gsl/manual/html_node/Beta-Functions.html#Beta-Functions
{
	double alpha;

	if(YAP_IsIntTerm(YAP_ARG1))
		alpha = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		alpha = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	double beta;

	if(YAP_IsIntTerm(YAP_ARG2))
		beta = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		beta = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;

	double res = gsl_sf_beta(alpha,beta);
	YAP_Term val = YAP_MkFloatTerm(res);
	return(YAP_Unify(YAP_ARG3,val));
}

static bool pl_logbetafunction(void)
{
	YAP_Term head, list = YAP_ARG1;

	list = YAP_ARG1;

	double sum=0;
	double logbetaf = 0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		double v;
		if(YAP_IsIntTerm(head))
			v = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			v = YAP_FloatOfTerm(head);
		else
			return false;

		list = YAP_TailOfTerm(list);
		sum+=v;
		logbetaf += gsl_sf_lngamma(v);
	}
	logbetaf -=gsl_sf_lngamma(sum);
	YAP_Term val = YAP_MkFloatTerm(logbetaf);
	return(YAP_Unify(YAP_ARG2,val));
}


static bool pl_student(void) // http://www.gnu.org/software/gsl/manual/html_node/The-t_002ddistribution.html
{
	double nu;

	if(YAP_IsIntTerm(YAP_ARG1))
		nu = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		nu = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	double sampled = gsl_ran_tdist(r,nu);
	YAP_Term val = YAP_MkFloatTerm(sampled);
	return(YAP_Unify(YAP_ARG2,val));
}
static bool pl_student_pdf(void) //args: nu, value, density http://www.gnu.org/software/gsl/manual/html_node/The-t_002ddistribution.html
{
	double nu;

	if(YAP_IsIntTerm(YAP_ARG1))
		nu = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		nu = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	double x;

	if(YAP_IsIntTerm(YAP_ARG2))
		x = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		x = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;

	double pdf = gsl_ran_tdist_pdf(x,nu);
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(pdf) ));
}
// likelihood for uniform distribution
static bool pl_uniformweight(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG2;
	int count=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		if(YAP_ExactlyEqual(head,YAP_ARG1))
			count++;
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension==0)
		return false;
	double w=1.0*count/dimension;
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(w) ));
}
// likelihood for finite distribution
static bool pl_finiteweight(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG2;
	double w=0.0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		YAP_Term p=YAP_ArgOfTerm(1,head);
		if(YAP_Unify(YAP_ArgOfTerm(2,head),YAP_ARG1))
		{
			double prob;
			if(YAP_IsIntTerm(p))
					prob = YAP_IntOfTerm(p);
				else if(YAP_IsFloatTerm(p))
					prob = YAP_FloatOfTerm(p);
				else
					return false;
			w+=prob;
		}
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension==0)
		return false;
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(w) ));
}

static bool pl_samplefinite(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	double w=0.0;
	vector<double> weights;
	vector<YAP_Term> values;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		YAP_Term p=YAP_ArgOfTerm(1,head);

		double prob;
		if(YAP_IsIntTerm(p))
				prob = YAP_IntOfTerm(p);
			else if(YAP_IsFloatTerm(p))
				prob = YAP_FloatOfTerm(p);
			else
				return false;
		weights.push_back(prob);
		values.push_back(YAP_ArgOfTerm(2,head));
		w+=prob;
		//cout<<prob<<endl;
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension==0)
		return false;
	discrete_distribution<int> distribution (weights.begin(),weights.end());
	int selectednode=distribution(generator);
	//	cout<<selectednode;
//	return true;
/*	list=YAP_ARG1;
	for(int i=0;i<selectednode;i++)
		list = YAP_TailOfTerm(list);
	head = YAP_HeadOfTerm(list);*/

	return(YAP_Unify(YAP_ARG2,values[selectednode] ));
}

static bool pl_listmax(void)
{
//	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	double maxvalue=0.0;
	YAP_Term maxvalueTerm=YAP_ArgOfTerm(1,YAP_HeadOfTerm(list));
	if(YAP_IsIntTerm(maxvalueTerm))
		maxvalue = YAP_IntOfTerm(maxvalueTerm);
	else if(YAP_IsFloatTerm(maxvalueTerm))
		maxvalue = YAP_FloatOfTerm(maxvalueTerm);
	else
		return false;


	YAP_Term maxlist = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	/*for(int a=dimension-1;a>=0;a--)
	{
		list_mean = YAP_MkPairTerm( YAP_MkFloatTerm(post_mean[a]), list_mean);
	}*/

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		YAP_Term p=YAP_ArgOfTerm(1,head);
		double value;
		if(YAP_IsIntTerm(p))
			value = YAP_IntOfTerm(p);
		else if(YAP_IsFloatTerm(p))
			value = YAP_FloatOfTerm(p);
		else
			return false;

		if(value>maxvalue)
		{
			maxvalue=value;
			maxlist = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
			YAP_Term term=YAP_ArgOfTerm(2,head);

			maxlist = YAP_MkPairTerm( term, maxlist);
		}
		else if(value==maxvalue)
		{
			YAP_Term term=YAP_ArgOfTerm(2,head);
			maxlist = YAP_MkPairTerm( term, maxlist);
		}

		list = YAP_TailOfTerm(list);
//		dimension++;
	}

	return(YAP_Unify(YAP_ARG2,maxlist));
}

static bool pl_weightedaverage(void)
{
//	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	double w=0.0;
	double average=0.0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		YAP_Term p=YAP_ArgOfTerm(1,head);
		double prob;
		if(YAP_IsIntTerm(p))
			prob = YAP_IntOfTerm(p);
		else if(YAP_IsFloatTerm(p))
			prob = YAP_FloatOfTerm(p);
		else
			return false;
		YAP_Term val=YAP_ArgOfTerm(2,head);
		double value;
		if(YAP_IsIntTerm(val))
			value = YAP_IntOfTerm(val);
		else if(YAP_IsFloatTerm(val))
			value = YAP_FloatOfTerm(val);
		else
			return false;

		average+=value*prob;
		w+=prob;
		list = YAP_TailOfTerm(list);
//		dimension++;
	}
	if(w==0)
		return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(w) ));
	average/=w;
	return(YAP_Unify(YAP_ARG2,YAP_MkFloatTerm(average)) && YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(w) ));
}

static bool pl_variancewis(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	double w=0.0;
	double average=0.0;
	double varianceIS=0.0;
	double variance=0.0;
	double varianceWIS=0.0;
	if(YAP_IsIntTerm(YAP_ARG2))
		average = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		average = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		YAP_Term p=YAP_ArgOfTerm(1,head);
		double prob;
		if(YAP_IsIntTerm(p))
			prob = YAP_IntOfTerm(p);
		else if(YAP_IsFloatTerm(p))
			prob = YAP_FloatOfTerm(p);
		else
			return false;
		YAP_Term val=YAP_ArgOfTerm(2,head);
		double value;
		if(YAP_IsIntTerm(val))
			value = YAP_IntOfTerm(val);
		else if(YAP_IsFloatTerm(val))
			value = YAP_FloatOfTerm(val);
		else
			return false;

		varianceWIS+=prob*prob*(value-average)*(value-average);
		varianceIS+=(value*prob-average)*(value*prob-average);
		variance+=prob*(value-average)*(value-average);
		w+=prob;
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if((w*w)==0 || dimension<=1)
		return false;
	varianceIS/=(dimension-1)*dimension;
	variance/=w;
	varianceWIS*=dimension/(dimension-1)/(w*w); // Monte Carlo Methods, with an emphasis on Bayesian computation pag.63 .  Advances in importance sampling pages 36-38; Weighted importance sampling techniques for monte carlo radiosity eq (5)
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(varianceIS) ) && YAP_Unify(YAP_ARG4,YAP_MkFloatTerm(varianceWIS)) && YAP_Unify(YAP_ARG5,YAP_MkFloatTerm(variance)));
}

// variance with known average
static bool pl_variance(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	double average=0.0;
	double variance=0.0;
	if(YAP_IsIntTerm(YAP_ARG2))
		average = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		average = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		double value;
		if(YAP_IsIntTerm(head))
			value = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			value = YAP_FloatOfTerm(head);
		else
			return false;

		variance+=(value-average)*(value-average);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	if(dimension==0)
		return false;
	variance/=(dimension);
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(variance) ));
}

static bool pl_averagevariance(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1,list2 = YAP_ARG1;
	double average=0.0;
	double variance=0.0;

	while(YAP_IsPairTerm(list2))
	{
		head = YAP_HeadOfTerm(list2);
		double value;
		if(YAP_IsIntTerm(head))
			value = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			value = YAP_FloatOfTerm(head);
		else
			return false;

		average+=value;
		list2 = YAP_TailOfTerm(list2);
		dimension++;
	}
	average/=dimension;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		double value;
		if(YAP_IsIntTerm(head))
			value = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			value = YAP_FloatOfTerm(head);
		else
			return false;

		variance+=(value-average)*(value-average);
		list = YAP_TailOfTerm(list);
	}
	if(dimension==0)
		return false;
	variance/=(dimension-1);
	return(YAP_Unify(YAP_ARG3,YAP_MkFloatTerm(variance) ) && YAP_Unify(YAP_ARG2,YAP_MkFloatTerm(average) ));
}

static bool pl_dirichlet(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	/*
	double alpha;
	
	if(YAP_IsIntTerm(YAP_ARG1))
		alpha = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		alpha = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;

	int dimension=1;
	if(YAP_IsIntTerm(YAP_ARG2))
		dimension = YAP_IntOfTerm(YAP_ARG2);
	*/

	//gsl_rng *r;

	size_t dim=dimension;
	double *dirParams= new double[dim];
	double *qNew= new double[dim];

	list = YAP_ARG1;

	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);

		if(YAP_IsFloatTerm(head))
			dirParams[i] = YAP_FloatOfTerm(head);
		else if(YAP_IsIntTerm(head))
			dirParams[i] = YAP_IntOfTerm(head);
		else
			return false;

		list = YAP_TailOfTerm(list);
		i++;
	}

	/*
	for(size_t i=0;i<dim;i++)
	{
		dirParams[i]= alpha;
		//qNew[i]=0;
	}
	*/
	gsl_ran_dirichlet(r, dim, dirParams, qNew);

	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(qNew[dimension-1]), YAP_MkAtomTerm(YAP_LookupAtom("[]")) );
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(qNew[a]), val);
	}
	delete[] dirParams;
	delete[] qNew;
	YAP_Term out = YAP_ARG2;
	return(YAP_Unify(out,val));
	/*
	YAP_Term out = YAP_ARG2;
	YAP_Term val= YAP_MkFloatTerm(float(qNew[0]));
	
	YAP_Term out2 = YAP_ARG3;
	YAP_Term val2= YAP_MkFloatTerm(float(qNew[1]));
	
	YAP_Term out3 = YAP_ARG4;
	YAP_Term val3= YAP_MkFloatTerm(float(qNew[2]));
	
	YAP_Term out4 = YAP_ARG5;
	YAP_Term val4= YAP_MkFloatTerm(float(qNew[3]));
	
	YAP_Term out5 = YAP_ARG6;
	YAP_Term val5= YAP_MkFloatTerm(float(qNew[4]));
	
	return(YAP_Unify(out,val) && YAP_Unify(out2,val2) && YAP_Unify(out3,val3) && YAP_Unify(out4,val4) && YAP_Unify(out5,val5) );
	*/
}


static bool pl_dirichlet_pdf(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;
	YAP_Term headv, listv = YAP_ARG2;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}


	//gsl_rng *r;

	size_t dim=dimension;
	double *dirParams= new double[dim];
	double *val= new double[dim];

	list = YAP_ARG1;

	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		headv = YAP_HeadOfTerm(listv);
		if(YAP_IsFloatTerm(head))
			dirParams[i] = YAP_FloatOfTerm(head);
		else if(YAP_IsIntTerm(head))
			dirParams[i] = YAP_IntOfTerm(head);
		else
			return false;

		if(YAP_IsFloatTerm(headv))
			val[i] = YAP_FloatOfTerm(headv);
		else if(YAP_IsIntTerm(headv))
			val[i] = YAP_IntOfTerm(headv);
		else
			return false;

		list = YAP_TailOfTerm(list);
		listv = YAP_TailOfTerm(listv);
		i++;
	}

	/*
	for(size_t i=0;i<dim;i++)
	{
		dirParams[i]= alpha;
		//qNew[i]=0;
	}
	*/
//	cout<<"=gsl_ran_dirichlet_pdf("<<dim<<","<< dirParams<<","<< val<<")";
	double pdf=gsl_ran_dirichlet_pdf(dim, dirParams, val);
//	cout<<pdf<<"=gsl_ran_dirichlet_pdf("<<dim<<","<< dirParams<<","<< val<<")";
	delete[] dirParams;
	delete[] val;
	YAP_Term out = YAP_ARG3;
	return(YAP_Unify(out,YAP_MkFloatTerm(pdf)));
	/*
	YAP_Term out = YAP_ARG2;
	YAP_Term val= YAP_MkFloatTerm(float(qNew[0]));

	YAP_Term out2 = YAP_ARG3;
	YAP_Term val2= YAP_MkFloatTerm(float(qNew[1]));

	YAP_Term out3 = YAP_ARG4;
	YAP_Term val3= YAP_MkFloatTerm(float(qNew[2]));

	YAP_Term out4 = YAP_ARG5;
	YAP_Term val4= YAP_MkFloatTerm(float(qNew[3]));

	YAP_Term out5 = YAP_ARG6;
	YAP_Term val5= YAP_MkFloatTerm(float(qNew[4]));

	return(YAP_Unify(out,val) && YAP_Unify(out2,val2) && YAP_Unify(out3,val3) && YAP_Unify(out4,val4) && YAP_Unify(out5,val5) );
	*/
}

static bool pl_normal(void)
{
  double mean;

  if(YAP_IsIntTerm(YAP_ARG1)){
    mean = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
    mean = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }

  double variance;
  if(YAP_IsIntTerm(YAP_ARG2)){
    variance = YAP_IntOfTerm(YAP_ARG2);
  }else  if(YAP_IsFloatTerm(YAP_ARG2)){
    variance = YAP_FloatOfTerm(YAP_ARG2);
  }else{
    return false;
  }
  
  SymmetricMatrix meas_noise_Cov(1);
  meas_noise_Cov(1,1) = variance;
	ColumnVector meas_noise_Mu(1);
	meas_noise_Mu(1) = mean;
  Gaussian gauss(meas_noise_Mu, meas_noise_Cov);
  Sample<ColumnVector> sample(1);
  gauss.SampleFrom(sample,DEFAULT,NULL);
  double rnd=sample.ValueGet()[0];
  
  YAP_Term out = YAP_ARG3;

  YAP_Term val= YAP_MkFloatTerm(rnd);
  return(YAP_Unify(out,val));
}

static bool pl_normalgsl(void)
{
  double mean;

  if(YAP_IsIntTerm(YAP_ARG1)){
    mean = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
    mean = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }

  double variance;
  if(YAP_IsIntTerm(YAP_ARG2)){
    variance = YAP_IntOfTerm(YAP_ARG2);
  }else  if(YAP_IsFloatTerm(YAP_ARG2)){
    variance = YAP_FloatOfTerm(YAP_ARG2);
  }else{
    return false;
  }


  double rnd=gsl_ran_gaussian(r,sqrt(variance))+mean;

  YAP_Term out = YAP_ARG3;

  YAP_Term val= YAP_MkFloatTerm(rnd);
  return(YAP_Unify(out,val));
}

ColumnVector termToColumnVector(int dimension,YAP_Term list)
{
	ColumnVector mean(dimension);
	int i=0;
	while(YAP_IsPairTerm(list))
	{
		YAP_Term head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	return mean;
}

Gaussian fillGauss(YAP_Term m,YAP_Term cov)
{
	int dimension=0;
	YAP_Term head, list = m;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	
	ColumnVector mean(dimension);
	SymmetricMatrix covariance(dimension);
	
	list = m;
	
	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	
	YAP_Term covMatrix = cov;
	for(int a=1;a<=dimension;a++)
	{
		for(int b=1;b<=dimension;b++)
		{
			head = YAP_HeadOfTerm(covMatrix);
			
			if(YAP_IsIntTerm(head))
				covariance(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				covariance(a,b) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}	
			covMatrix = YAP_TailOfTerm(covMatrix);
		}
	}
	
			
	Gaussian gauss(mean, covariance);
	return gauss;
}

/* not finished
mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov

VarQ=0.1, DeltaT=0.1, Cov11 is VarQ*(DeltaT^4)/4, Cov12 is VarQ*(DeltaT^3)/2, Cov21 is VarQ*(DeltaT^3)/2, Cov22 is VarQ*(DeltaT^2),
kalman([1,0],[0.2,0,0,0.1],[1,DeltaT,0,1],[0,0],[0,0],[Cov11,Cov12,Cov21,Cov22], [1,0] , [0],[0.2],[0],[1.2], A,B),
kalman(A,B,[1,DeltaT,0,1],[0,0],[0,0],[Cov11,Cov12,Cov21,Cov22], [1,0] , [0],[0.2],[0],[1.3], M,Cov).

kalman([1],[0.2],[1],[1],[0.0],[0.1],[1],[0.0],[0.1],[2],[1],A,B).
*/
static bool pl_kalman(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	
	cout<<"dim "<<dimension<<endl;
	Gaussian prior = fillGauss(YAP_ARG1,YAP_ARG2);
	cout<<prior<<endl;
	
	Matrix A(dimension,dimension);
	list = YAP_ARG3;
	
	for(int r=1;r<=dimension;r++)
	{
		for(int c=1;c<=dimension;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);
			
			//YAP_Write(head,do_putc,0);
		
			if(YAP_IsIntTerm(head))
				A(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				A(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}

			list = YAP_TailOfTerm(list);
		}
	}
	
	cout<<"A "<<A<<endl;
	
	int input_dim=0;
	list = YAP_ARG4;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		input_dim++;
	}
	input_dim/=dimension;
	
	
	
	Matrix B(dimension,input_dim);
	list = YAP_ARG4;
	for(int r=1;r<=dimension;r++)
	{
		for(int c=1;c<=input_dim;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);
		
			if(YAP_IsIntTerm(head))
				B(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				B(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}
		
			list = YAP_TailOfTerm(list);
		}
	}

	cout<<"B "<<B<<endl;
	
	vector<Matrix> AB(2);
	AB[0] = A;
	AB[1] = B;
	Gaussian system_Uncertainty=fillGauss(YAP_ARG5,YAP_ARG6);
	
	cout<<"system_Uncertainty "<<system_Uncertainty<<endl;
	
	// create the model
	LinearAnalyticConditionalGaussian sys_pdf(AB, system_Uncertainty);
	LinearAnalyticSystemModelGaussianUncertainty sys_model(&sys_pdf);
	
	int output_dim=0;
	list = YAP_ARG7;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		output_dim++;
	}
	output_dim/=dimension;
	
	Matrix H(output_dim,dimension);
	list = YAP_ARG7;
	for(int r=1;r<=output_dim;r++)
	{
		for(int c=1;c<=dimension;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);
		
			if(YAP_IsIntTerm(head))
				H(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				H(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}
		
			list = YAP_TailOfTerm(list);
		}
	}
	
	cout<<"H "<<H<<endl;
	
	// Construct the measurement noise
	Gaussian measurement_Uncertainty=fillGauss(YAP_ARG8,YAP_ARG9);

	cout<<"measurement_Uncertainty "<<measurement_Uncertainty<<endl;
	// create the model
	LinearAnalyticConditionalGaussian meas_pdf(H, measurement_Uncertainty);
	LinearAnalyticMeasurementModelGaussianUncertainty meas_model(&meas_pdf);
	
	ExtendedKalmanFilter filter(&prior);
	
	ColumnVector input = termToColumnVector(input_dim,YAP_ARG10);
	cout<<"input "<<input<<endl;
	
	ColumnVector measurement = termToColumnVector(output_dim,YAP_ARG11);
	cout<<"measurement "<<measurement<<endl;
	
	filter.Update(&sys_model,input,&meas_model,measurement);
	
	
	ColumnVector post_mean = filter.PostGet()->ExpectedValueGet();
	SymmetricMatrix post_cov =  filter.PostGet()->CovarianceGet();
	
	cout<<"post_mean"<<post_mean<<endl;
	cout<<"post_cov"<<post_cov<<endl;
	

	YAP_Term list_mean = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension-1;a>=0;a--)
	{
		list_mean = YAP_MkPairTerm( YAP_MkFloatTerm(post_mean[a]), list_mean);
	}

	cout<<"list_mean ";
	//YAP_Write(list_mean,do_putc,0); cout<<endl;

	YAP_Term out_m = YAP_ARG12;

	YAP_Term list_cov = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension;a>=1;a--)
	{
		for(int b=dimension;b>=1;b--)
		{
			list_cov = YAP_MkPairTerm( YAP_MkFloatTerm(post_cov(a,b)), list_cov);
		}
	}

	cout<<"list_cov ";
	//YAP_Write(list_cov,do_putc,0); cout<<endl;

	YAP_Term out_cov = YAP_ARG13;
//	YAP_Term val = YAP_MkIntTerm(filter.PostGet()[0].DimensionGet() );
	return(YAP_Unify(out_m,list_mean) && YAP_Unify(out_cov,list_cov));

}

// mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov,Weight

static bool pl_kalmanRao(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}

	cout<<"dim "<<dimension<<endl;
	Gaussian prior = fillGauss(YAP_ARG1,YAP_ARG2);
	cout<<prior<<endl;

	Matrix A(dimension,dimension);
	list = YAP_ARG3;

	for(int r=1;r<=dimension;r++)
	{
		for(int c=1;c<=dimension;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);

			//YAP_Write(head,do_putc,0);

			if(YAP_IsIntTerm(head))
				A(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				A(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}

			list = YAP_TailOfTerm(list);
		}
	}

	cout<<"A "<<A<<endl;

	int input_dim=0;
	list = YAP_ARG4;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		input_dim++;
	}
	input_dim/=dimension;



	Matrix B(dimension,input_dim);
	list = YAP_ARG4;
	for(int r=1;r<=dimension;r++)
	{
		for(int c=1;c<=input_dim;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);

			if(YAP_IsIntTerm(head))
				B(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				B(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}

			list = YAP_TailOfTerm(list);
		}
	}

	cout<<"B "<<B<<endl;

	vector<Matrix> AB(2);
	AB[0] = A;
	AB[1] = B;
	Gaussian system_Uncertainty=fillGauss(YAP_ARG5,YAP_ARG6);

	cout<<"system_Uncertainty "<<system_Uncertainty<<endl;

	// create the model
	LinearAnalyticConditionalGaussian sys_pdf(AB, system_Uncertainty);
	LinearAnalyticSystemModelGaussianUncertainty sys_model(&sys_pdf);

	int output_dim=0;
	list = YAP_ARG7;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		output_dim++;
	}
	output_dim/=dimension;

	Matrix H(output_dim,dimension);
	list = YAP_ARG7;
	for(int r=1;r<=output_dim;r++)
	{
		for(int c=1;c<=dimension;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);

			if(YAP_IsIntTerm(head))
				H(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				H(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}

			list = YAP_TailOfTerm(list);
		}
	}

	cout<<"H "<<H<<endl;

	// Construct the measurement noise
	Gaussian measurement_Uncertainty=fillGauss(YAP_ARG8,YAP_ARG9);

	cout<<"measurement_Uncertainty "<<measurement_Uncertainty<<endl;
	// create the model
	LinearAnalyticConditionalGaussian meas_pdf(H, measurement_Uncertainty);
	LinearAnalyticMeasurementModelGaussianUncertainty meas_model(&meas_pdf);

	ExtendedKalmanFilter filter(&prior);

	ColumnVector input = termToColumnVector(input_dim,YAP_ARG10);
	cout<<"input "<<input<<endl;

	filter.Update(&sys_model,input);

	ColumnVector post_mean = filter.PostGet()->ExpectedValueGet();
	SymmetricMatrix post_cov =  filter.PostGet()->CovarianceGet();

	double weight = 1.0;
	//if there is a measurement
	if(YAP_IsPairTerm(YAP_ARG11))
	{
		ColumnVector measurement = termToColumnVector(output_dim,YAP_ARG11);
		cout<<"measurement "<<measurement<<endl;
		// for rao-black

		ColumnVector rao_mean = H*post_mean;
		cout<<"rao_mean"<<rao_mean<<endl;
		Matrix postHT =   (Matrix)(post_cov) * H.transpose() ;
		Matrix S_Matrix =  H * postHT;
		S_Matrix += (Matrix) measurement_Uncertainty.CovarianceGet();

		SymmetricMatrix rao_cov;
		S_Matrix.convertToSymmetricMatrix(rao_cov);
		cout<<"rao_cov"<<S_Matrix<<endl;
		Gaussian raoGauss(rao_mean, rao_cov);

		//computing the weight
		weight = raoGauss.ProbabilityGet(measurement).getValue();

		// ---

		//update the model according to the last measurement
		filter.Update(&meas_model,measurement);

		post_mean = filter.PostGet()->ExpectedValueGet();
		post_cov =  filter.PostGet()->CovarianceGet();
	}

	cout<<"post_mean"<<post_mean<<endl;
	cout<<"post_cov"<<post_cov<<endl;



	YAP_Term list_mean = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension-1;a>=0;a--)
	{
		list_mean = YAP_MkPairTerm( YAP_MkFloatTerm(post_mean[a]), list_mean);
	}
	
	cout<<"list_mean ";
	//YAP_Write(list_mean,do_putc,0); cout<<endl;
	
	YAP_Term out_m = YAP_ARG12;
	
	YAP_Term list_cov = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension;a>=1;a--)
	{
		for(int b=dimension;b>=1;b--)
		{
			list_cov = YAP_MkPairTerm( YAP_MkFloatTerm(post_cov(a,b)), list_cov);
		}
	}
	
	cout<<"list_cov ";
	//YAP_Write(list_cov,do_putc,0); cout<<endl;
	
	YAP_Term out_cov = YAP_ARG13;

	YAP_Term out_weight = YAP_ARG14;
//	YAP_Term val = YAP_MkIntTerm(filter.PostGet()[0].DimensionGet() );

	return(YAP_Unify(out_m,list_mean) && YAP_Unify(out_cov,list_cov) && YAP_Unify(out_weight,YAP_MkFloatTerm(weight) ) );

}

//1 mean_Xt, 2 cov_Xt, 3 H, 4 mean_measurement_uncertainty, 5 cov_measurement_uncertainty, 6 measurement, 7 post mean, 8 post cov, 9 Weight

static bool pl_kalmanRao_simplified(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}

	//cout<<"dim "<<dimension<<endl;
	Gaussian prior = fillGauss(YAP_ARG1,YAP_ARG2);
	//cout<<prior<<endl;

	int output_dim=0;
	list = YAP_ARG3;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		output_dim++;
	}
	output_dim/=dimension;

	Matrix H(output_dim,dimension);
	list = YAP_ARG3;
	for(int r=1;r<=output_dim;r++)
	{
		for(int c=1;c<=dimension;c++)
		{
			if(!YAP_IsPairTerm(list))
			{
				cout<<"error";
				return false;
			}
			head = YAP_HeadOfTerm(list);

			if(YAP_IsIntTerm(head))
				H(r,c) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				H(r,c) = YAP_FloatOfTerm(head);
			else
			{
				cout<<"error";
				return false;
			}

			list = YAP_TailOfTerm(list);
		}
	}

	//cout<<"H "<<H<<endl;

	// Construct the measurement noise
	Gaussian measurement_Uncertainty=fillGauss(YAP_ARG4,YAP_ARG5);

	//cout<<"measurement_Uncertainty "<<measurement_Uncertainty<<endl;
	// create the model
	LinearAnalyticConditionalGaussian meas_pdf(H, measurement_Uncertainty);
	LinearAnalyticMeasurementModelGaussianUncertainty meas_model(&meas_pdf);

	ExtendedKalmanFilter filter(&prior);

	ColumnVector post_mean = filter.PostGet()->ExpectedValueGet();
	SymmetricMatrix post_cov =  filter.PostGet()->CovarianceGet();

	double weight = 1.0;
	//if there is a measurement
	//YAP_Write(YAP_ARG6,do_putc,0); cout<<endl;
	//cout<<"output_dim "<<output_dim<<endl;
	if(YAP_IsPairTerm(YAP_ARG6))
	{
		ColumnVector measurement = termToColumnVector(output_dim,YAP_ARG6);
		//cout<<"measurement "<<measurement<<endl;
		// for rao-black

		ColumnVector rao_mean = H*post_mean;
		//cout<<"rao_mean"<<rao_mean<<endl;
		Matrix postHT =   (Matrix)(post_cov) * H.transpose() ;
		Matrix S_Matrix =  H * postHT;
		S_Matrix += (Matrix) measurement_Uncertainty.CovarianceGet();

		SymmetricMatrix rao_cov;
		S_Matrix.convertToSymmetricMatrix(rao_cov);
		//cout<<"rao_cov"<<S_Matrix<<endl;
		Gaussian raoGauss(rao_mean, rao_cov);

		//computing the weight
		// the weight is p(y_t|...), see 18.3.1.4 Posterior predictive (Machine learning: a probabilistic perspective page 642)
		weight = raoGauss.ProbabilityGet(measurement).getValue();
		//cout<<"weight"<<weight<<endl;
		// ---

		//update the model according to the last measurement
		filter.Update(&meas_model,measurement);

		post_mean = filter.PostGet()->ExpectedValueGet();
		post_cov =  filter.PostGet()->CovarianceGet();
	}

	//cout<<"post_mean"<<post_mean<<endl;
	//cout<<"post_cov"<<post_cov<<endl;



	YAP_Term list_mean = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension-1;a>=0;a--)
	{
		list_mean = YAP_MkPairTerm( YAP_MkFloatTerm(post_mean[a]), list_mean);
	}

	//cout<<"list_mean ";
	//YAP_Write(list_mean,do_putc,0); cout<<endl;

	YAP_Term out_m = YAP_ARG7;

	YAP_Term list_cov = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
	for(int a=dimension;a>=1;a--)
	{
		for(int b=dimension;b>=1;b--)
		{
			list_cov = YAP_MkPairTerm( YAP_MkFloatTerm(post_cov(a,b)), list_cov);
		}
	}

	//cout<<"list_cov ";
	//YAP_Write(list_cov,do_putc,0); cout<<endl;

	YAP_Term out_cov = YAP_ARG8;

	YAP_Term out_weight = YAP_ARG9;
//	YAP_Term val = YAP_MkIntTerm(filter.PostGet()[0].DimensionGet() );

	return(YAP_Unify(out_m,list_mean) && YAP_Unify(out_cov,list_cov) && YAP_Unify(out_weight,YAP_MkFloatTerm(weight) ) );

}

// densityGaussian in prolog: compute the density of a Gaussian
static bool pl_densityMultivariateGaussian(void)
{
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
	}
	
	ColumnVector mean(dimension);
	SymmetricMatrix covariance(dimension);
	
	list = YAP_ARG1;
	
	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			mean[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			mean[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	
	YAP_Term covMatrix = YAP_ARG2;
	for(int a=1;a<=dimension;a++)
	{
		for(int b=1;b<=dimension;b++)
		{
			head = YAP_HeadOfTerm(covMatrix);
			
			if(YAP_IsIntTerm(head))
				covariance(a,b) = YAP_IntOfTerm(head);
			else if(YAP_IsFloatTerm(head))
				covariance(a,b) = YAP_FloatOfTerm(head);
			else
				return false;
				
			covMatrix = YAP_TailOfTerm(covMatrix);
		}
	}
	
			
	Gaussian gauss(mean, covariance);
	
	ColumnVector sample(dimension);
	
	list = YAP_ARG3;
	
	i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		
		if(YAP_IsIntTerm(head))
			sample[i] = YAP_IntOfTerm(head);
		else if(YAP_IsFloatTerm(head))
			sample[i] = YAP_FloatOfTerm(head);
		else
			return false;
		
		list = YAP_TailOfTerm(list);
		i++;
	}
	
	double density = gauss.ProbabilityGet(sample).getValue();

	YAP_Term out = YAP_ARG4;
	YAP_Term val = YAP_MkFloatTerm(density);
	return(YAP_Unify(out,val));
	/*
	ColumnVector mean(2);
	
	if(YAP_IsIntTerm(YAP_ARG1))
		mean[0] = YAP_IntOfTerm(YAP_ARG1);
	else if(YAP_IsFloatTerm(YAP_ARG1))
		mean[0] = YAP_FloatOfTerm(YAP_ARG1);
	else
		return false;
  
	if(YAP_IsIntTerm(YAP_ARG2))
		mean[1] = YAP_IntOfTerm(YAP_ARG2);
	else if(YAP_IsFloatTerm(YAP_ARG2))
		mean[1] = YAP_FloatOfTerm(YAP_ARG2);
	else
		return false;

	SymmetricMatrix covariance(2);
  
	if(YAP_IsIntTerm(YAP_ARG3))
		covariance(1,1) = YAP_IntOfTerm(YAP_ARG3);
	else if(YAP_IsFloatTerm(YAP_ARG3))
		covariance(1,1) = YAP_FloatOfTerm(YAP_ARG3);
	else
		return false;
    
	if(YAP_IsIntTerm(YAP_ARG4))
		covariance(1,2) = YAP_IntOfTerm(YAP_ARG4);
	else if(YAP_IsFloatTerm(YAP_ARG4))
		covariance(1,2) = YAP_FloatOfTerm(YAP_ARG4);
	else
		return false;
  
  if(YAP_IsIntTerm(YAP_ARG5))
		covariance(2,1) = YAP_IntOfTerm(YAP_ARG5);
	else if(YAP_IsFloatTerm(YAP_ARG5))
		covariance(2,1) = YAP_FloatOfTerm(YAP_ARG5);
	else
		return false;
  
  if(YAP_IsIntTerm(YAP_ARG6))
		covariance(2,2) = YAP_IntOfTerm(YAP_ARG6);
	else if(YAP_IsFloatTerm(YAP_ARG6))
		covariance(2,2) = YAP_FloatOfTerm(YAP_ARG6);
	else
		return false;
		
	Gaussian gauss(mean, covariance);
	
	ColumnVector sample(2);
	
	if(YAP_IsIntTerm(YAP_ARG7))
		sample[0] = YAP_IntOfTerm(YAP_ARG7);
	else if(YAP_IsFloatTerm(YAP_ARG7))
		sample[0] = YAP_FloatOfTerm(YAP_ARG7);
	else
		return false;
	if(YAP_IsIntTerm(YAP_ARG8))
		sample[1] = YAP_IntOfTerm(YAP_ARG8);
	else if(YAP_IsFloatTerm(YAP_ARG8))
		sample[1] = YAP_FloatOfTerm(YAP_ARG8);
	else
		return false;
		
	double density = gauss.ProbabilityGet(sample).getValue();

	YAP_Term out = YAP_ARG9;
	YAP_Term val = YAP_MkFloatTerm(density);
	return(YAP_Unify(out,val));
	*/
}

/* USES RNG
static bool pl_poisson(void)
{
  double lambda;

  if(YAP_IsIntTerm(YAP_ARG1)){
    lambda = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
    lambda = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }
  RNG x;
  int rnd= x.poisson(lambda);
  YAP_Term out = YAP_ARG2;

  YAP_Term val= YAP_MkIntTerm(rnd);
  return(YAP_Unify(out,val));
}
*/

static bool pl_poisson(void)
{
  double lambda;

  if(YAP_IsIntTerm(YAP_ARG1)){
    lambda = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
    lambda = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }
  int rnd = gsl_ran_poisson(r,lambda);
  YAP_Term out = YAP_ARG2;

  YAP_Term val= YAP_MkIntTerm(rnd);
  return(YAP_Unify(out,val));
}

static bool pl_poissonpdf(void)
{
	double k;

	if(YAP_IsIntTerm(YAP_ARG1)){
		k = YAP_IntOfTerm(YAP_ARG1);
	}else  if(YAP_IsFloatTerm(YAP_ARG1)){
		k = YAP_FloatOfTerm(YAP_ARG1);
	}else{
		return false;
	}
	double lambda;

	if(YAP_IsIntTerm(YAP_ARG2)){
		lambda = YAP_IntOfTerm(YAP_ARG2);
	}else  if(YAP_IsFloatTerm(YAP_ARG2)){
		lambda = YAP_FloatOfTerm(YAP_ARG2);
	}else{
		return false;
	}
  double pdf = gsl_ran_poisson_pdf(k,lambda);
  YAP_Term out = YAP_ARG3;

  YAP_Term val= YAP_MkFloatTerm(pdf);
  return(YAP_Unify(out,val));
}

static bool pl_betapdf(void)
{
	double x,a,b;

	if(YAP_IsIntTerm(YAP_ARG1)){
		x = YAP_IntOfTerm(YAP_ARG1);
	}else  if(YAP_IsFloatTerm(YAP_ARG1)){
		x = YAP_FloatOfTerm(YAP_ARG1);
	}else{
		return false;
	}

	if(YAP_IsIntTerm(YAP_ARG2)){
		a = YAP_IntOfTerm(YAP_ARG2);
	}else  if(YAP_IsFloatTerm(YAP_ARG2)){
		a = YAP_FloatOfTerm(YAP_ARG2);
	}else{
		return false;
	}

	if(YAP_IsIntTerm(YAP_ARG3)){
		b = YAP_IntOfTerm(YAP_ARG3);
	}else  if(YAP_IsFloatTerm(YAP_ARG3)){
		b = YAP_FloatOfTerm(YAP_ARG3);
	}else{
		return false;
	}
  double pdf = gsl_ran_beta_pdf(x,a,b);
  YAP_Term out = YAP_ARG4;

  YAP_Term val= YAP_MkFloatTerm(pdf);
  return(YAP_Unify(out,val));
}


static bool pl_checkgoallw(void)
{
	YAP_Functor ground=YAP_MkFunctor(YAP_LookupAtom("ground"),1);
	int dimension=0;
	YAP_Term head, list = YAP_ARG1;

	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);
		list = YAP_TailOfTerm(list);
		dimension++;
		if(YAP_RunGoalOnce(YAP_MkApplTerm(ground,1,&head)))
		{
//			cout<<"ground"<<YAP_RunGoalOnce(YAP_MkApplTerm(ground,1,&head))<<endl;
			bool un=YAP_Unify(head,YAP_ARG2);
/*			cout<<"unification "<<dimension<<" "<<un<<endl;
			// cout<<"ground"<<YAP_RunGoalOnce(YAP_MkApplTerm(ground,1,&head))<<endl;
			YAP_Term TermList[1];
			TermList[0]=head;
			YAP_RunGoalOnce(YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("writeln"),1),1,TermList));
			TermList[0]=YAP_ARG2;
			YAP_RunGoalOnce(YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("writeln"),1),1,TermList));
			cout<<endl;
*/
			if(un)
				return true;
		}
	}
/*

	size_t dim=dimension;
	double *dirParams= new double[dim];
	double *qNew= new double[dim];

	list = YAP_ARG1;

	int i=0;
	while(YAP_IsPairTerm(list))
	{
		head = YAP_HeadOfTerm(list);

		if(YAP_IsFloatTerm(head))
			dirParams[i] = YAP_FloatOfTerm(head);
		else if(YAP_IsIntTerm(head))
			dirParams[i] = YAP_IntOfTerm(head);
		else
			return false;

		list = YAP_TailOfTerm(list);
		i++;
	}

	gsl_ran_dirichlet(r, dim, dirParams, qNew);

	YAP_Term val = YAP_MkPairTerm(YAP_MkFloatTerm(qNew[dimension-1]), YAP_MkAtomTerm(YAP_LookupAtom("[]")) );
	for(int a=dimension-2;a>=0;a--)
	{
		val = YAP_MkPairTerm( YAP_MkFloatTerm(qNew[a]), val);
	}

	YAP_Term out = YAP_ARG2;*/
	return(false);

}


static bool pl_setseed(void)
{
  double seed;

  if(YAP_IsIntTerm(YAP_ARG1)){
	  seed = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
	  seed = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }
  Boost_Rng.seed(seed);
  gsl_rng_set(r,seed);
  return(true);
}
/*
// draws 2 independent gaussian values
static bool pl_normal2(void)
{
  double mean;

  if(YAP_IsIntTerm(YAP_ARG1)){
    mean = YAP_IntOfTerm(YAP_ARG1);
  }else  if(YAP_IsFloatTerm(YAP_ARG1)){
    mean = YAP_FloatOfTerm(YAP_ARG1);
  }else{
    return false;
  }

  double variance;
  if(YAP_IsIntTerm(YAP_ARG2)){
    variance = YAP_IntOfTerm(YAP_ARG2);
  }else  if(YAP_IsFloatTerm(YAP_ARG2)){
    variance = YAP_FloatOfTerm(YAP_ARG2);
  }else{
    return false;
  }
  
  double mean2;

  if(YAP_IsIntTerm(YAP_ARG3)){
    mean2 = YAP_IntOfTerm(YAP_ARG3);
  }else  if(YAP_IsFloatTerm(YAP_ARG3)){
    mean2 = YAP_FloatOfTerm(YAP_ARG3);
  }else{
    return false;
  }

  double variance2;
  if(YAP_IsIntTerm(YAP_ARG4)){
    variance2 = YAP_IntOfTerm(YAP_ARG4);
  }else  if(YAP_IsFloatTerm(YAP_ARG4)){
    variance2 = YAP_FloatOfTerm(YAP_ARG4);
  }else{
    return false;
  }
  
  RNG x; 
  double rnd= x.normal(mean,sqrt(variance));
  YAP_Term out = YAP_ARG5;
  
  RNG x2; 
  double rnd2= x2.normal(mean2,sqrt(variance2));
  YAP_Term out2 = YAP_ARG6;

  YAP_Term val= YAP_MkFloatTerm(rnd);
  YAP_Term val2= YAP_MkFloatTerm(rnd2);
  return(YAP_Unify(out,val) && YAP_Unify(out2,val2));
}
*/

extern "C"{
  void init_my_predicates()
  {
    initrand();
    YAP_UserCPredicate("discreteuniform",pl_discreteuniform,2);
    YAP_UserCPredicate("samplefinite",pl_samplefinite,2);
  	YAP_UserCPredicate("betafunction",pl_betafunction,3);
  	YAP_UserCPredicate("logbetafunction",pl_logbetafunction,2);
    YAP_UserCPredicate("poisson",pl_poisson,2);
    YAP_UserCPredicate("poissonPdf",pl_poissonpdf,3);
    YAP_UserCPredicate("betaPdf",pl_betapdf,4);
    YAP_UserCPredicate("normal",pl_normal,3);
    YAP_UserCPredicate("normalgsl",pl_normalgsl,3);
	YAP_UserCPredicate("gamma",pl_gamma,3);
	YAP_UserCPredicate("gammaPdf",pl_gamma_pdf,4);
	YAP_UserCPredicate("student",pl_student,2);
	YAP_UserCPredicate("studentPdf",pl_student_pdf,3);
//	YAP_UserCPredicate("normal2",pl_normal2,6);
	YAP_UserCPredicate("densityGaussian",pl_densityMultivariateGaussian,4);
	YAP_UserCPredicate("gaussian",pl_multivariateGaussian,3);
	YAP_UserCPredicate("dirichlet",pl_dirichlet,2);
	YAP_UserCPredicate("dirichletPdf",pl_dirichlet_pdf,3);
	YAP_UserCPredicate("kalman",pl_kalman,13);
	YAP_UserCPredicate("kalmanrao",pl_kalmanRao,14);
	YAP_UserCPredicate("kalmanrao_simplified",pl_kalmanRao_simplified,9);
	YAP_UserCPredicate("optimalproposal",pl_proposalDistribution,7);
	YAP_UserCPredicate("matrixproduct",pl_matrixproduct,4);
	YAP_UserCPredicate("setseed",pl_setseed,1);
	YAP_UserCPredicate("uniformweight",pl_uniformweight,3);
	YAP_UserCPredicate("finiteweight",pl_finiteweight,3);
	YAP_UserCPredicate("weightedaverage",pl_weightedaverage,3);
	YAP_UserCPredicate("variance",pl_variance,3);
	YAP_UserCPredicate("avgvar",pl_averagevariance,3);
	YAP_UserCPredicate("variance_wis",pl_variancewis,5);
	YAP_UserCPredicate("initmap",pl_initmap,1);
	YAP_UserCPredicate("addvaluemap",pl_addvaluemap,3);
	YAP_UserCPredicate("averagemap",pl_averagemap,2);
	YAP_UserCPredicate("deletemap",pl_deletemap,1);
	YAP_UserCPredicate("listmax",pl_listmax,2);
	YAP_UserCPredicate("checkgoallw",pl_checkgoallw,3);
  }
}
