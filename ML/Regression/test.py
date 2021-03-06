#!/bin/env python
#-*- coding:utf8

import regression
import helper
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

def testAbalone():
    abX,abY = regression.loadDataSet('abalone.txt')

    # Locally Weighted Linear Regression
    # smaller kernel will give a lower error, but it will overfit our data
    yHat01 = regression.lwlrTest(abX[0:99], abX[0:99], abY[0:99], 0.1)
    yHat1 = regression.lwlrTest(abX[0:99], abX[0:99], abY[0:99], 1)
    yHat10 = regression.lwlrTest(abX[0:99], abX[0:99], abY[0:99], 10)

    print("rssError-01:", regression.rssError(abY[0:99], yHat01.T))
    print("rssError-1:", regression.rssError(abY[0:99], yHat1.T))
    print("rssError-10:", regression.rssError(abY[0:99], yHat10.T))


    yHat01 = regression.lwlrTest(abX[100:199], abX[100:199], abY[100:199], 0.1)
    yHat1 = regression.lwlrTest(abX[100:199], abX[100:199], abY[100:199], 1)
    yHat10 = regression.lwlrTest(abX[100:199], abX[100:199], abY[100:199], 10)

    print("rssError-01:", regression.rssError(abY[100:199], yHat01.T))
    print("rssError-1:", regression.rssError(abY[100:199], yHat1.T))
    print("rssError-10:", regression.rssError(abY[100:199], yHat10.T))

    # Simple Linear regression
    ws = regression.standRegres(abX[0:99], abY[0:99])
    yHat = mat(abX[100:199])*ws
    print("rssError-Simp:", regression.rssError(abY[100:199], yHat.T.A))

def testRidge():
    abX,abY = regression.loadDataSet('abalone.txt')
    ridgeWeights = regression.ridgeTest(abX, abY)
    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(ridgeWeights)
    plt.show()

def testStage():
    xArr,yArr = regression.loadDataSet('abalone.txt')
    print("stageWise-0.01-200:", regression.stageWise(xArr, yArr, 0.01, 200))
    print("stageWise-0.001-500:", regression.stageWise(xArr, yArr, 0.001, 500))

    xMat = mat(xArr); yMat = mat(yArr).T
    xMat = regression.regularize(xMat)
    yM = mean(yMat, 0)
    yMat = yMat - yM
    weights = regression.standRegres(xMat, yMat.T)
    print("normal-linear:", weights.T)

def testLEG():
    lgX = []; lgY = []
    helper.setDataCollect(lgX, lgY)

if __name__ == '__main__':
    #test()
    #test2()
    #testAbalone()
    #testRidge()
    #testStage()
    testLEG()
