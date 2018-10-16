#!/bin/bash
source common.sh

extractValue "$($CHAINDATA_VIEWER --prefix LastHeader)"
