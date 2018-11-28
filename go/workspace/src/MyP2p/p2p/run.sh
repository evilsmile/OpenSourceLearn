#!/bin/bash

peer_port="$1"
hash="$2"

local_port=$((peer_port+1))

go run main.go -l $local_port -d /ip4/127.0.0.1/tcp/$peer_port/ipfs/$hash
