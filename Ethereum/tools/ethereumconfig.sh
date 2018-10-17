#!/bin/bash
source common.sh

extractValue "$($CHAINDATA_VIEWER --prefix ethereum-config)" | json_reformat
