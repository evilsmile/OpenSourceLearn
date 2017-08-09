FLEX=flex
BISON=bison
CXXFLAG=-g -Wall

all: binary

binary: json_convertor

json_convertor: j.y.o  j.l.o \
	test.o easytdata.o
	g++  ${CXXFLAG}  -o $@ $^
 
.cpp.o:
	g++ ${CXXFLAG} -c -o $@ $^

j.l.cpp:j.l
	${FLEX} -o $@ $< #-P"json2tdata_" 

j.y.cpp:j.y
	${BISON} -d -o $@ $^ #-p"json2tdata_"

clean:
	rm -f *.o j.y.cpp j.y.hpp j.l.cpp
