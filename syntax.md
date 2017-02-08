**DC Syntax (work in progress)**

Comments as in Prolog: %

***deterministic clauses***
Syntax: head := body.
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
coin2 ~ finite([0.2:true,0.8:false]) := coin ~= true.
```

***operations***

unification for random variables
Syntax: variable ~= term

Examples:
```
coin ~= true
```

***Supported distributions***

Bernoulli/categorical

syntax ```finite([probability1:value1,...,probabilityn:valuen])```

example ```finite([0.1:black,0.5:red,0.4:blue])```

uniform categorical (discrete)

syntax ```uniform([value1,value2,...,valuen])```

example ```uniform([black,red,blue])```



