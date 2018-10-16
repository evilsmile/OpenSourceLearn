#!/bin/bash

HEX=$(echo $1 | tr '[a-z]' '[A-Z]'])

echo "ibase=16;$HEX" | bc
