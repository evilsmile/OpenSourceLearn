include ../make.comm

BIN=consumer producer logger

all:${BIN}

consumer:consumer.o ${DEP_LIBS_COMMENT}
	${CC} ${CXXFLAGS} -o $@ $< ${LDFLAGS}

logger:logger.o ${DEP_LIBS_COMMENT}
	${CC} ${CXXFLAGS} -o $@ $< ${LDFLAGS}

producer:producer.o  ${DEP_LIBS_COMMENT}
	${CC} ${CXXFLAGS} -o $@ $< ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
