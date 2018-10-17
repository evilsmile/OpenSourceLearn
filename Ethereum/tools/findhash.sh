#!/bin/bash
source common.sh

hash="$1"
$CHAINDATA_VIEWER --hash $hash
