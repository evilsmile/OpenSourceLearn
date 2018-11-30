GOFILES=$(shell ls *.go)
BIN=multipro

all: protos multipro

.PHONY:protos
	
${BIN}:${GOFILES}
	export GOPATH=`pwd`/../../:$$GOPATH && go build -o $@ $^

protos:
	/opt/protobuf/bin/protoc --go_out=. pb/*.proto

clean:
	rm -f pb/*.pb.go ${BIN}
