# Distributional Clauses
A probabilistic logic language for hybrid relational domains

DC: Distributional Clauses for static models  
DCPF: Distributional Clauses Particle Filter for filtering in dynamic models
## Prerequisites
Install Yap prolog (tested on YAP 6.2.3 and [YAP 6.3.3](http://www.dcc.fc.up.pt/~vsc/Yap/yap-6.3.3.tar.gz)), follow these instructions and using these options for configure:
```
../configure --enable-tabling=yes --enable-dynamic-loading
```

Install GSL library e.g. for Ubuntu:
```
sudo apt-get install libgsl0-dev libgsl0ldbl
```
Install boost library

## Compilation
Execute
```
sh make.sh
```
## Test
Execute
```
sh make.sh
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