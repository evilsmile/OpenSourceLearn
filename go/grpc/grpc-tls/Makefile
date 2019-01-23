.PHONY: proto

all:proto


# --go_out=.: 设置Go代码生成目录 
# plugins参数传递给protoc-gen-geo,告诉它请支持RPC,
proto:
	cd proto/ && protoc --go_out=plugins=grpc:. *.proto
