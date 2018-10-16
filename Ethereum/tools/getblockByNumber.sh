#!/bin/bash

source common.sh

number=$1
$CHAINDATA_VIEWER -prefix h --number $number
#extractValue "$($CHAINDATA_VIEWER -prefix h --number $number)"
