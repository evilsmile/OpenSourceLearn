CXXFLAGS=-g -I.
PUB_LIBS=libtutorial.a
LIBS=-lprotobuf

BINS=address_write address_read 

all: gen_proto $(PUB_LIBS) $(BINS)

gen_proto:tutorial.proto
	protoc -I=. --cpp_out=. $^
	touch gen_proto

address_write:address_write.o
	libtool --tag=CC --mode=link g++ -g -o $@ $^ $(PUB_LIBS) $(LIBS)

address_read:address_read.o
	libtool --tag=CC --mode=link g++ -g -o $@ $^ $(PUB_LIBS) $(LIBS)

%.o:%.cpp
	libtool --tag=CC --mode=compile g++ -g -c $^

%.o:%.cc 
	libtool --tag=CC --mode=compile g++ -g -c $^

libtutorial.a:tutorial.pb.o
	ar crs $@ $^

tutorial.pb.o:tutorial.pb.cc 
	g++ -g -c $^

clean: 
	libtool --mode=clean rm -f *.o $(BINS) $(PUB_LIBS) gen_proto tutorial.pb.*
