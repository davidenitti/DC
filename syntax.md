#Introduction to DC (work in progress)

DC is based on Yap-prolog. This means that the model and inference are written in a Prolog file.

###Initialization
The first part of the program regards library inclusion and initialization. For example:
```
:- use_module('../distributionalclause.pl'). % load distributional clauses library
:- use_module('../random/sampling.pl'). % load sampling library
:- use_module(library(lists)). % load list library (prolog)

:- set_options(default),set_query_propagation(true). % options
:- initialization(init). % initialize DC
```
###Options
Set default options: ```:- set_options(default).```

Set default options + query propagation ```:- set_options(default),set_query_propagation(true).```

###Model
The model is described using distributional clauses. The syntax is explained in the next section.

Example:
```
% define a discrete random variable (bernoulli)
coin ~ finite([0.2:true,0.8:false]). % syntax name_variable ~ finite([prob:value,prob:value,...])
```

###Inference
Inference is called with the predicate ```query```

Syntax: ``` query(positive_evidence_List,negative_evidence_List,query_to_consider,num_samples,Probability). ```

Example:
Given the file model.pl
```
:- use_module('../distributionalclause.pl'). % load distributional clauses library
:- use_module('../random/sampling.pl'). % load sampling library
:- use_module(library(lists)). % load list library (prolog)
:- set_options(default). % to enable query propagation replace the line with :- set_options(default),set_query_propagation(true).

:- initialization(init). % initialize DC

% define a discrete random variable (bernoulli)
coin ~ finite([0.2:true,0.8:false]). % syntax name_variable ~ finite([prob:value,prob:value,...])
```

call yap -l model.pl and write in the prompt:
```
query([],[],coin ~= true,100,P). % 100 samples
```

Output

```
P = 0.2
```


#DC Syntax


**Built-in Yap-Prolog operators**
* numerical comparisons between numbers (not random variables): `=`, `<`, `>`, `<=`, `>=`
* equality operator (unification): `=`
* equality operator with expression evaluation: `is`

 For example, `10 is 2*5` is true, `A is 2*5` is true for `A=10` (the logical variable A is unified with 10 if possible)
 
* integer number comparison: `between(A,B,C)` C is between A and B and C is integer.

 For example, the formula `between(1,5,C)` is true for C=1, 2, 3, 4, 5.
* single-line comments: `%`


##Deterministic clauses

Syntax: `head := body.`

Explaination: body implies head as clauses in prolog, the only difference is the implication operator := instead of :-
The body is a list of literals (atomic formulas or their negations).

Example:
```
not_empty(Y) := inside(X,Y). % not_empty(Y) is true if there exists an object X such that inside(X,Y) is true.

inside(1,2) := true. % inside(1,2) is a true fact. The body has to be specified to distinguish it with prolog syntax
```


##Distributional clauses

Syntax: `head ~ distribution := body.`

Explaination: if 'body' is true the random variable head is defined with a given distribution.

Multiple DC clauses can define the same random variable, as long as the bodies are mutually exclusive.

If none of the bodies that define head is true, the random variable head is not defined.

Examples:
```
coin ~ finite([0.2:true,0.8:false]). % the body is implicitly true.
coin2 ~ finite([0.4:true,0.6:false]) := coin ~= true. % if coin is true then coin2 is defined with a given distribution.
```

##Operators


**Equality operator for random variables**

Syntax: `var ~= term`

Explaination: the value of the random variable 'var' is equal to term (unifiable). A term can be a constant, logical variable or a compund term.

Examples:
```
tcoin := coin ~= true. % 'tcoin' is true if the value of coin is the constant term 'true'
anyvalue := coin ~= A. % 'anyvalue' is true if the random variable coin exists (whatever is its value). More formally, anyvalue is true iff exists A such that the value of coin is equal to A.
samevalue := coin ~= A, coin2 ~= A. % 'samevalue' is true if the values of coin and coin2 are the same. More formally, 'samevalue' is true iff exists A such that the value of coin and coin2 are both equal to A.
```

**Other comparison operators**

At the moment the operators <, >, <=, >= are defined only between constants (not random variables).

To compare the value of a random variable the equality operator is used first.

Examples:
```
person(carl) := true.
person(bob) := true.
height(X) ~ gaussian(1.65,0.1) := person(X). % for each person X, height(x) is a random variable with gaussian distribution
gender(X) ~ finite([0.54:female,0.46:male]) := person(X).

tall(X) := height(X) ~= H,H>2. % tall(X) is true iff the value of height(X) is greater than 2. 
not_tall(X) := person(X), \+ tall(X). % not_tall(X) is true if X is a not tall person 
```
The definition of 'tall(X)' is deterministic, but since it depends on the random variable 'height(X)', 'tall(X)' is implicitely a random variable. The atom `tall(X)` or its negation `\+ tall(X)` can be used as literals in the body of another clause.

###Supported distributions

* Bernoulli/categorical

 syntax ```finite([probability1:value1,...,probabilityn:valuen])```

 example ```finite([0.1:black,0.5:red,0.4:blue])```
* Uniform categorical (discrete)

 syntax ```uniform([value1,value2,...,valuen])```

 example ```uniform([black,red,blue])```
* Gaussian (univariate)

 syntax ```gaussian(mean,variance)```

 example ```gaussian(0,1)```
* Gaussian (multivariate)

 syntax ```gaussian(meanvector,covariance)```

 example ```gaussian([0,1],[1,0.1,0.1,1])``` % the covariance is flatten row by row
 
* Beta

 syntax ```beta(a,b)```

 example ```beta(1,2)``` % beta distribution with alpha=1 and beta=2
* to finish...
