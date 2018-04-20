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
    myTree = tree.createTree(myDat, labels)

    # persistenting the decision tree
    tree.storeTree(myTree, 'myTree.train')

    myTree2 = tree.grabTree('myTree.train')
    testVec = [1, 0]
    print "Test ",testVec," result: ", tree.classify(myTree2, labels, testVec)
    testVec = [1, 1]
    print "Test ",testVec," result: ", tree.classify(myTree2, labels, testVec)

def testClass2():
    fr = open('lenses.txt')
    lenses = [inst.strip().split('\t') for inst in fr.readlines()]
    lensesLabels =  ['age', 'prescript', 'astigmatic', 'tearRate']
    lensesTree = tree.createTree(lenses, lensesLabels)
    print lensesTree

if __name__ == '__main__':
    #testEntr()
    #testClass()
    testClass2()
