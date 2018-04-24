#!/bin/env python
#-*- coding:utf8

from numpy import *

def loadDataSet(fileName):
    numFeat = len(open(fileName).readline().split('\t'))-1
    dataMat = []; labelMat = []
    fr = open(fileName)
    for line in fr.readlines():
        lineArr = []
        curLine = line.strip().split('\t')
        for i in range(numFeat):
            lineArr.append(float(curLine[i]))
        dataMat.append(lineArr)
        labelMat.append(float(curLine[-1]))
    return dataMat, labelMat

# Compute best-fit line.
def standRegres(xArr, yArr):
    xMat = mat(xArr); yMat = mat(yArr).T
    xTx = xMat.T*xMat
    # compute determinate
    if linalg.det(xTx) == 0.0:
        print("This matrix is singular, cannot do inverse")
        return
    # ws is weights
    ws = xTx.I * (xMat.T * yMat)

    return ws

# Create matrics from input data.
# k controls how quickly the decay happens.
def lwlr(testPoint, xArr, yArr, k=1.0):
    xMat = mat(xArr); yMat = mat(yArr).T
    m = shape(xMat)[0]
    # Create diagonal matrix
    # weights matrix is a square matrix with as many elements as data points.
    weights = mat(eye((m)))
    # Populate weights with exponentially decaying values
    for j in range(m):
        diffMat = testPoint - xMat[j,:]
        weights[j,j] = exp(diffMat*diffMat.T/(-2.0*k**2))
    xTx = xMat.T * (weights * xMat)
    if linalg.det(xTx) == 0.0:
        print("This matrix is singular, cannot do inverse")
        return

    ws = xTx.I * (xMat.T * (weights * yMat))
    return testPoint * ws

# call lwlr() for every point in the dataset.
def lwlrTest(testArr, xArr, yArr, k=1.0):
    m = shape(testArr)[0]
    yHat = zeros(m)
    for i in range(m):
        yHat[i] = lwlr(testArr[i], xArr, yArr, k)
    return yHat

def rssError(yArr, yHatArr):
    return ((yArr-yHatArr)**2).sum()

################ Ridge Regression #################
# Calculate weights
def ridgeRegres(xMat, yMat, lam=0.2):
    xTx = xMat.T*xMat
    denom = xTx + eye(shape(xMat)[1])*lam
    if linalg.det(denom) == 0.0:
        print("This matrix is singular, cannot do inverse")
        return
    ws = denom.I * (xMat.T*yMat)
    return ws

# Test over a number of lambda value
def ridgeTest(xArr, yArr):
    xMat = mat(xArr); yMat = mat(yArr).T
    # Regulation
    yMean = mean(yMat, 0)
    yMat = yMat - yMean
    xMeans = mean(xMat, 0)
    xVar = var(xMat, 0)
    xMat = (xMat - xMeans)/xVar
    numTestPts = 30
    wMat = zeros((numTestPts, shape(xMat)[1]))
    for i in range(numTestPts):
        ws = ridgeRegres(xMat, yMat, exp(i-10))
        wMat[i,:]=ws.T
    return wMat
