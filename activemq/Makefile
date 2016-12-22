LDFLAGS=-lactivemq-cpp
CXXFLAGS=-g -I/usr/local/include/activemq-cpp-3.9.3

SRC=$(shell ls *.cpp)
OBJECT=$(patsubst %.cpp,%.o,${SRC})

BIN=activemq_topic_test

all: ${BIN}

${BIN}:${OBJECT}
	g++ ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -rf  *.o ${BIN} core.*
