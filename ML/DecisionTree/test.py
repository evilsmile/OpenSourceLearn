#!/bin/env python
#-*- coding:utf8

import tree

# 0.970950594455 
def testEntr():
    myDat, labels = tree.createDataSet()
    print tree.calcShannonEnt(myDat)

#{'no surfacing': {0: 'no', 1: {'flippers': {0: 'no', 1: 'yes'}}}}
def testClass():
    myDat, labels = tree.createDataSet()
    print tree.createTree(myDat, labels)

if __name__ == '__main__':
    #testEntr()
    testClass()
