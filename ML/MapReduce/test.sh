#!/bin/bash

#python mrMean.py --mapper < inputFile.txt > out.tmp
#python mrMean.py --reduce < out.tmp
#python mrMean.py < inputFile.txt

python mrSVM.py --mapper < inputFile.txt > out.tmp
python mrSVM.py --reduce < out.tmp
