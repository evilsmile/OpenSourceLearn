#!/bin/env python
#-*- coding:utf8

import cart
from numpy import *

def test():
    import matplotlib.pyplot as plt
    myDat = cart.loadDataSet('ex00.txt')
    myMat = mat(myDat)
    tree = cart.createTree(myMat)
    print("createTree:", tree)

    plt.plot(myMat[:,0], myMat[:,1], 'ro')
    plt.show()

    myDat1 = cart.loadDataSet("ex0.txt")
    myMat1 = mat(myDat1)
    print("createTree1:", cart.createTree(myMat1))
    plt.plot(myMat1[:,0], myMat1[:,1], 'ro')
    plt.show()

    myDat2 = cart.loadDataSet("ex2.txt")
    myMat2 = mat(myDat2)
    print("createTree2:", cart.createTree(myMat2))
    plt.plot(myMat2[:,0], myMat2[:,1], 'ro')
    plt.show()

        
 
if __name__ == '__main__':
    test()
