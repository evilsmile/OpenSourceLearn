#!/bin/env python
#-*- coding:utf8

import fpGrowth
from numpy import *

def test():
    rootNode = fpGrowth.treeNode('pyramid', 9, None)
    rootNode.children['eye'] = fpGrowth.treeNode('eye', 13, None)
    rootNode.children['phoeix'] = fpGrowth.treeNode('phoeix', 3, None)
    rootNode.disp()
   
def test2():
    simplDat = fpGrowth.loadSimpleData()
    print "Data: ", simplDat
    initSet = fpGrowth.createInitSet(simplDat)
    print "initSet: ", initSet
    myFPtree, myHeaderTab = fpGrowth.createTree(initSet, 3)
    myFPtree.disp()
    condPat = fpGrowth.findPrefixPath('x', myHeaderTab['x'][1])
    print "condPat: ", condPat

    freqItems = []
    fpGrowth.mineTree(myFPtree, myHeaderTab, 3, set([]), freqItems)
    print "freqItems: ", freqItems

def test3():
    parsedDat = [line.split() for line in open('kosarak.dat').readlines()]
    initSet = fpGrowth.createInitSet(parsedDat)
    myFPtree, myHeaderTab = fpGrowth.createTree(initSet, 10000)
    myFreqList = []
    fpGrowth.mineTree(myFPtree, myHeaderTab, 10000, set([]), myFreqList)
    print myFreqList

if __name__ == '__main__':
    #test()
    test2()
    #test3()
