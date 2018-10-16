#!/bin/bash
source common.sh

#$CHAINDATA_VIEWER --prefix LastBlock | sed 's/.*Value\[\(.*\)\]/\1/g'

extractValue "$($CHAINDATA_VIEWER --prefix LastBlock)"
