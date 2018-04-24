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
   
if __name__ == '__main__':
    test()
