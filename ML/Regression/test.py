#!/bin/env python
#-*- coding:utf8

import regression
from numpy import *

def test():
    xArr, yArr = regression.loadDataSet('ex0.txt')
    print("xArr:", xArr, " yArr:", yArr)

    ws = regression.standRegres(xArr, yArr)
    print("ws:", ws)

    xMat = mat(xArr)
    yMat = mat(yArr)
    yHat = xMat * ws

    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(xMat[:,1].flatten().A[0], yMat.T[:,0].flatten().A[0])

    xCopy = xMat.copy()
    xCopy.sort(0)
    yHat = xCopy*ws
    ax.plot(xCopy[:,1], yHat)
    plt.show()
   

def test2():
    xArr, yArr = regression.loadDataSet('ex0.txt')
    print("lwlr-1.0:",regression.lwlr(xArr[0], xArr, yArr, 1.0))
    print("lwlr-0.001:",regression.lwlr(xArr[0], xArr, yArr, 0.001))

    yHat = regression.lwlrTest(xArr, xArr, yArr, 0.01)
    print("yHat:", yHat)
    xMat = mat(xArr)
    srtIdx = xMat[:,1].argsort(0)
    xSort = xMat[srtIdx][:,0,:]

    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(xSort[:,1], yHat[srtIdx])
    ax.scatter(xMat[:,1].flatten().A[0], mat(yArr).T.flatten().A[0], s=2, c='red')
    plt.show()

if __name__ == '__main__':
    #test()
    test2()
