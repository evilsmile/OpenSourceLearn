#!/bin/bash

blockBPM=555
if [ $# -gt 0 ]; then
    blockBPM=$1
fi

echo "Submit a new block with BPM: $blockBPM"
curl localhost:8001 -d '{"BPM":'$blockBPM'}'

echo -e "\n================ New blocklist ============"
curl localhost:8001
