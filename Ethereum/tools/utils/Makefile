BIN=ethereum_chaindata_leveldb_viewer rlp_tool
all: ${BIN}
ethereum_chaindata_leveldb_viewer:ethereum_chaindata_leveldb_viewer.go
	go build $^

rlp_tool:rlp_tool.go
	go build $^

clean:
	rm -f ${BIN}
