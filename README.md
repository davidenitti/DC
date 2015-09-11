# Distributional Clauses (beta)
Probabilistic logic language for inference, planning and learning in static and dynamic domains  
**DC**: Distributional Clauses for inference in static models.  
> DC example and tutorial at [examples/tutorial.pl](https://github.com/davidenitti/DC/blob/master/examples/tutorial.pl)  
**DCPF**: Distributional Clauses Particle Filter for filtering in dynamic models  
**HYPE**: planner for hybrid MDPs based on DCPF (the code will be soon available)  
**HybRel**: planner for hybrid relational MDPs based on DCPF (the code not yet available)   

The code is in beta, if you need help or find a bug please write an [issue](https://github.com/davidenitti/DC/issues)
or contact me at davide (_DOT_) nitti (AT) cs (_DOT_) kuleuven (_DOT_) be

## Prerequisites
Install Yap prolog (tested on [YAP 6.3.4](https://github.com/vscosta/yap-6.3/archive/master.zip)), follow these instructions:
```
cd yap-6.3
mkdir arch
cd arch
../configure --enable-tabling=yes --enable-dynamic-loading
make
sudo make install
sudo make install_library
```

Install GSL library, e.g., for Ubuntu:
```
sudo apt-get install libgsl0-dev libgsl0ldbl
```
Install boost library, e.g., for Ubuntu:
```
sudo apt-get install libboost-all-dev
```

## Compilation
Execute
```
sh make.sh
```
## Test
Execute
```
sh test.sh
```
or equivalently
```
yap -L examples/example1.pl
```
Expected output (the error values may differ):
```
Testing example1.pl...
Absolute error drawn(1) ~= 1: 4.91269841260689e-05
Absolute error drawn(1) ~= 2: 0.000294206349202047
Absolute error drawn(1) ~= 3: 0.00178948412698662
Absolute error average g ~ Gaussian(0,0.1): 0.000900261603206224
% 0.290 CPU in 0.291 seconds ( 99% CPU)
```
## Tutorials
### DC tutorial
A quick DC tutorial is available at [examples/tutorial.pl](https://github.com/davidenitti/DC/blob/master/examples/tutorial.pl)  
To run the tutorial, execute inside examples: 
```
yap -l tutorial.pl
```
then write test_coin(N). followed by enter or other queries as explained in the file
