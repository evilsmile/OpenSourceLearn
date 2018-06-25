.phony: clean

CXXFLAGS=-g --std=c++11
LDFLAGS=-pthread -levent
BINS=$(shell ls *cpp | sed 's/\.cpp//g')

all:${BINS}
	@echo ${BINS} | sed 's/ /\r\n/g' > .gitignore

%:%.cpp
	g++ ${CXXFLAGS} -o $@ $<  ${LDFLAGS}

clean:
	rm -f ${BINS}
