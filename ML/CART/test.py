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
    
if __name__ == '__main__':
    test()
