BINS=server client
all: protos ${BINS}

.PHONY: protos

protos:
	@for subdir in $$(find  protos/ -maxdepth 1 -mindepth 1 -type d); do   \
		echo "generate protos under $$subdir.. ";                          \
		cd $$subdir;                                                       \
	    protoc --go_out=plugins=grpc:. *.proto;                            \
		cd -;     \
	done

server: server.go

client:client.go

%:%.go
	go build $^

clean:
	rm -f ${BINS} $$(find . -name "*.pb.go")
