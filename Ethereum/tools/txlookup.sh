#!/bin/bash
source common.sh

txhash=$1

eval $($RLPTOOL --txlookup $(extractValue "$($CHAINDATA_VIEWER --prefix l --hash $txhash)") | awk '{printf("blockhash=%s;blocknum=%d;index=%d", $1, $2, $3)}')
echo "BlockHash: $blockhash"
echo "BlockNumber: $blocknum"
echo "TxIndex: $index"
