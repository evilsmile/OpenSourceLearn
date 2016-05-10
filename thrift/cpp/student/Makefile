BIN=server
GENDIR=gen-cpp
GEN_SRCS=#$(shell ls ${GENDIR}/*)
SRCS=
CPPFLAGS=-g -DHAVE_NETINET_IN_H -I. -I/home/ian//freeworld_programs/thrift-0.9.3/include/
LDFLAGS=-L/home/ian/freeworld_programs/thrift-0.9.3/lib 
CC=g++

all: gen_cpp server client

gen_cpp: student.thrift
	thrift -r --gen cpp $<

#	${CC} ${CPPFLAGS} ${LDFLAGS}  ${GENDIR}/* $^ -o $@ -lssl -levent -lthriftnb -lthrift  -levent -lrt
server: server.cpp gen-cpp/Serv.cpp gen-cpp/student_types.cpp gen-cpp/student_constants.cpp
	${CC} ${CPPFLAGS} ${LDFLAGS} $^ -o $@ -lssl -lthriftnb -levent -lthrift -lrt

client: client.cpp
	${CC} ${CPPFLAGS} ${LDFLAGS}  gen-cpp/Serv.cpp gen-cpp/student_types.cpp gen-cpp/student_constants.cpp $^ -o $@ -lpthread -lthrift -lrt -lssl

clean:
	$(RM) -rf *.o ${GENDIR} client server
