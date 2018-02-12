.phony: clean run

try_record.beam:try_record.erl
	erlc -o $@ $<

# run function 'test' of module 'try_record', and exit by 'stop' of 'init'
run:
	erl -noshell -s try_record test -s init stop

clean:
	rm -f *.dump *beam
