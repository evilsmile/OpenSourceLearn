NAME=gs_test
BEAM_NAME=$(patsubst %,%.beam,${NAME})
ERL_NAME=$(patsubst %,%.erl,${NAME})
ERLC=erlc
ERL=erl -noshell

${BEAM_NAME}:${ERL_NAME}
	${ERLC} -o $@ $<

run:
	${ERL} -s ${NAME} start_link -s ${NAME} test -s init stop

clean:
	rm -f *dump
