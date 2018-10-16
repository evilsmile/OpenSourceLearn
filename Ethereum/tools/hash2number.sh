#!/bin/bash

source common.sh

blockhash=$1

hexnumber=$(extractValue "$($CHAINDATA_VIEWER -prefix H -hash $blockhash)")

h2d.sh $hexnumber
