#!/bin/env python
#-*- coding:utf8

import cart
from numpy import *

show_pic=False

def test():
    myDat = cart.loadDataSet('ex00.txt')
    myMat = mat(myDat)
    tree = cart.createTree(myMat)
    print("createTree:", tree)

    if show_pic == True:
        import matplotlib.pyplot as plt
        plt.plot(myMat[:,0], myMat[:,1], 'ro')
        plt.show()

    myDat1 = cart.loadDataSet("ex0.txt")
    myMat1 = mat(myDat1)
    print("createTree1:", cart.createTree(myMat1))
    if show_pic == True:
        plt.plot(myMat1[:,0], myMat1[:,1], 'ro')
        plt.show()

    myDat2 = cart.loadDataSet("ex2.txt")
    myMat2 = mat(myDat2)
    myTree = cart.createTree(myMat2, ops=(0,1))
    print("Tree:", myTree)
    if show_pic == True:
        plt.plot(myMat2[:,0], myMat2[:,1], 'ro')
        plt.show()

    myDatTest = cart.loadDataSet('ex2test.txt')
    myMat2Test = mat(myDatTest)
    myPruneTree = cart.prune(myTree, myMat2Test)

    print("PruneTree:", myPruneTree)
        
    myDatTest = cart.loadDataSet('exp2.txt')
    myMat2Test = mat(myDatTest)
    myModelTree = cart.createTree(myMat2, cart.modelLeaf, cart.modelErr, (1,10))
    if show_pic == True:
        plt.plot(myMat2[:,0], myMat2[:,1], 'ro')
        plt.show()
    print("ModelTree:", myModelTree)

    
def testBike():
    trainMat = mat(cart.loadDataSet('bikeSpeedVsIq_train.txt'))
    testMat = mat(cart.loadDataSet('bikeSpeedVsIq_test.txt'))
    myTree = cart.createTree(trainMat, ops=(1,20))
    yHat = cart.createForeCast(myTree, testMat[:,0])
    print corrcoef(yHat, testMat[:,1], rowvar=0)[0,1]
 
if __name__ == '__main__':
    #test()
    testBike()
