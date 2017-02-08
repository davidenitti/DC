**DC Syntax (work in progress)**

Comments as in Prolog: %

***Deterministic clauses***

Syntax: `head := body.`

Explaination: body implies head as clauses in prolog, the only difference is the implication operator := instead of :-

Example:
```
notempty(Y) := inside(X,Y).

inside(1,2) := true. % the body has to be specified to distinguish with prolog syntax
```

***Distributional clauses***

Syntax: head ~ distribution := body.

Explaination: if 'body' is true the random variable head is defined with a given distribution.

Multiple DC clauses can define the same random variable, as long as the bodies are mutually exclusive.

If none of the bodies that define head is true, the random variable head is not defined.

Examples:
```
coin ~ finite([0.2:true,0.8:false]). % the body is implicitly true.
coin2 ~ finite([0.4:true,0.6:false]) := coin ~= true. % if coin is true then coin2 is defined with a given distribution.
```

***operations***

equality operator for random variables
Syntax: var ~= term

Explaination: the value of the random variable 'var' is equal to term (unifiable). A term can be a constant, logical variable or a compund term.

Examples:
```
tcoin := coin ~= true. % 'tcoin' is true if the value of coin is the constant term 'true'
anyvalue := coin ~= A. % 'anyvalue' is true if the random variable coin exists (whatever is its value). More formally, anyvalue is true iff exists A such that the value of coin is equal to A.
samevalue := coin ~= A, coin2 ~= A. % 'samevalue' is true if the values of coin and coin2 are the same. More formally, 'samevalue' is true iff exists A such that the value of coin and coin2 are both equal to A.
```

***Supported distributions***

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
