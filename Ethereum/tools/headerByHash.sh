#!/bin/bash

source common.sh

hash=$1

# to hex
num=$(d2h.sh $(./hash2number.sh $hash))
# get key-value record
# extract value part
# send it to rlp_tool
# filter rlp_tool log
# get json part
# output json pretty format
$RLPTOOL -head $(extractValue "$($CHAINDATA_VIEWER -prefix h -number $num -hash $hash \
    | grep 'header data')") 2>/dev/null                                                 \
    | sed 's/.*header json: \({.*\)/\1/g'                                               \
    | json_reformat
