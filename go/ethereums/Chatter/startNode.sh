#!/bin/bash

ETHERDIR=/root/GitRepos/Others/go-ethereum 
StartBootCmd="build/bin/bootnode -nodekey=bootnode.key -verbosity=8"
GetBootKey="build/bin/bootnode -nodekey=bootnode.key -writeaddress"

Port=5435
if [ $# -gt 0 ]; then
    Port=$1
fi

running=$(ps aux | grep "$StartBootCmd" | grep -v grep | wc -l)
if [ $running -eq 1 ]; then 
    echo "Boot node already running."
else
    echo "Start boot-node .... "
    cd $ETHERDIR && $StartBootCmd 1>/dev/null 2>&1 &
    echo "Start boot-node done"
fi

pubkey=$(cd $ETHERDIR && $GetBootKey)
localaddr=$(ifconfig | grep inet | grep -vw 127| awk '{print $2}')

bootNodeInfo="enode://${pubkey}@${localaddr}:30301"
echo "Start at [:$Port]. Init connection to '$bootNodeInfo'"
./chatter -port $Port -bootnode $bootNodeInfo
