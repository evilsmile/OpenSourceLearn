#!/bin/env python
#-*- coding:utf8

import logicReg
from numpy import *

def test():
    dataMat, labelMat = logicReg.loadDataSet()
    print("dataMat:",dataMat)
    print("labelMat:",labelMat)
    #weights = logicReg.gradAscent(dataMat, labelMat)
    #weights=weights.getA()
    #weights = logicReg.stocGradAscent0(array(dataMat), labelMat)
    weights = logicReg.stocGradAscent1(array(dataMat), labelMat)
    print("weights:",weights)
    logicReg.plotBestFit(weights)

def colicTest():
    frTrain = open("horseColicTraining.txt")
    frTest = open('horseColicTest.txt')
    trainingSet = []; trainingLabels = []
    for line in frTrain.readlines():
        currLine = line.strip().split('\t')
        lineArr = []
        for i in range(21):
            lineArr.append(float(currLine[i]))
        trainingSet.append(lineArr)
        trainingLabels.append(float(currLine[21]))
    trainWeights = logicReg.stocGradAscent1(array(trainingSet), trainingLabels, 500)
    errorCount = 0; numTestVec = 0.0
    for line in frTest.readlines():
        numTestVec += 1.0
        currLine = line.strip().split('\t')
        lineArr = []
        for i in range(21):
            lineArr.append(float(currLine[i]))
        if (int(logicReg.classifyVector(array(lineArr), trainWeights)) != int(currLine[21])):
            errorCount += 1
    errorRate = (float(errorCount)/numTestVec)
    print "the error rate of this test is: %f" % errorRate
    return errorRate

def multiTest():
    numTests = 10; errorSum = 0.0
    for k in range(numTests):
        errorSum += colicTest()
    print "after %d iterations the average error rate is: %f" % (numTests, errorSum/float(numTests))

if __name__ == '__main__':
    #test()
    multiTest()
