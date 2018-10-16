#!/bin/bash
#set -x
source common.sh

hash=$1

# to hex
num=$(d2h.sh $(./hash2number.sh $hash))
# get key-value record
# extract value part
# filter rlp_tool log
# get json part
# output json pretty format
$RLPTOOL -receipt $(extractValue "$($CHAINDATA_VIEWER -prefix r -number $num -hash $hash)") | sed 's/.*receipt json\[.\]: \(.*\)/\1/g' | json_reformat
