#!/bin/bash

protoc -I/usr/local/include -I. -I$GOPATH/src/github.com/grpc-hello-world/proto/google/api --swagger_out=logtostderr=true:. ./hello.proto
