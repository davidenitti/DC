# FLAGS = -g -fPIC -Wall
# LIBDIR = -Llib
# LIBS = 
# CC = g++
# include 

# DYNAMIC = -dynamic
# SUPPRESS = -flat_namespace -undefined suppress
 



SHELL=/bin/sh
USRLIBDIR=-Llibunix
#LIBS = -lobj -lcudd -lmtr -lst -lutil -lepd
CFLAGS= -std=c++0x -shared -fPIC -ffast-math -Wall -Os -march=native
DEBUGOPTIONS=-g -O0 

#OBJDIR=svm-Dlight
#OBJECTS = rng.o
# OBJECTS = countainer.o\
# 	YapHelpers.o
# kfoil_main.o \
#	kfoil_yap_interface.o \
#	Task.o \
#	MulticlassKernelTargetAlignment.o \
#	utility.o \
#	$(OBJDIR)/svm_common.o \
#	$(OBJDIR)/plugin.o \
#	$(OBJDIR)/sparsevector.o \
#	$(OBJDIR)/svm_learn.o \
#	$(OBJDIR)/svm_hideo.o 


TARGETS=sampling.so 

all: static

shared: sampling.o
	g++ -o sampling.so sampling.o -shared $(USRLIBDIR) -L/usr/local/lib -lorocos-bfl -lYap -lgsl -lgslcblas -lm -fPIC

static: sampling.o
	g++ -o sampling.so sampling.o liborocos-bfl.a -shared -lYap -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC

allstatic: sampling.o
	g++ -o sampling.so sampling.o liborocos-bfl.a libYap.a -shared -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC

static32: sampling.o
	g++ -m32 -o sampling.so sampling.o liborocos-bfl_32.a -shared -lYap -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC

sharedlocal: sampling.o
	g++ -o sampling.so sampling.o -shared -L./ -Wl,-rpath=./ -lorocos-bfl -lYap -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC # -Ibfl

# $(DEBUGOPTIONS)  
%.o:%.cpp
	g++ ${CFLAGS} -c $< -o $@ 	

%.o:%.c
	gcc ${CFLAGS} -c $< -o $@

clean:
	rm -f *.o *~ ${TARGETS}


test: sampling.so
	echo "load_foreign_files(['sampling'],[],init_my_predicates). gaussian([1],[1],[Val]), write('sample from gaussian(1,1): '),writeln(Val),halt. halt(1)." | yap

#sampling.so : sampling.o
#	g++ -o sampling.so sampling.o -shared -L./ -Wl,-rpath=./ -lorocos-bfl-patch -lYap -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC 

#install: all
#	 cp ${TARGETS} ~/COMMON/lib/

# 	g++ -o sampling.so sampling.o -shared $(USRLIBDIR) -lorocos-bfl -L/usr/local/lib -lYap -lgsl -lgslcblas -lm -fPIC 

#	 g++ $(USRLIBDIR)  $(LIBS) -p -shared -o $@ $+  

#g++ -o sampling.so sampling.o -shared -Llib -lobj -lcudd -lmtr -lst -lutil -lepd
#static: sampling.o
#	g++ -o sampling.so sampling.o -shared -L./ -Wl,-rpath=./ -lorocos-bfl -lYap -L/usr/local/lib $(USRLIBDIR) -lgsl -lgslcblas -lm -fPIC
