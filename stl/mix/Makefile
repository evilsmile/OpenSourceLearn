.phony: clean

CXXFLAGS=-g --std=c++11
BINS=test test_forward test_move test_reffold test_mystr_Rref test_var_temp test_valuetype

all:${BINS}
	@echo ${BINS} | sed 's/ /\r\n/g' > .gitignore

%:%.cpp
	g++ ${CXXFLAGS} -o $@ $< 

clean:
	rm -f ${BINS}
