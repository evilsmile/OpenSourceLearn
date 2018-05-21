#!/bin/env python
#-*- coding:utf8

import apriori
from numpy import *

def test():
    dataSet = apriori.loadDataSet()
    L,suppData = apriori.apriori(dataSet)
    rules = apriori.generateRules(L, suppData, minConf=0.5)
    print rules

def test2():
    mushDataSet = [line.split() for line in open('mushroom.dat').readlines()]
    L,suppData = apriori.apriori(mushDataSet, minSupport=0.3)
    for item in L[1]:
        if item.intersection('2'):
            print item
    
if __name__ == '__main__':
    #test()
    test2()
