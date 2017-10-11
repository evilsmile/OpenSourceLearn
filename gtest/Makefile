CXXFLAGS=-g -Wall
CC=g++

OBJS=func.o

GTEST_INC=-Igoogletest/googletest/include/
GTEST_LIBS=googletest/googletest/libgtest.a googletest/googletest/libgtest_main.a

test:funcTest.o ${OBJS}
	${CC} ${CXXFLAGS} ${GTEST_INC}  -o $@ $^ ${GTEST_LIBS} -lpthread

.cpp.o:
	${CC} ${CXXFLAGS} -c -o $@ $^

clean:
	rm -f test *.o
