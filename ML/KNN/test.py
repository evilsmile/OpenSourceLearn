#!/bin/env python
#-*- coding:utf8

import KNN
from numpy import *
from os import *

def test():
    group,labels = KNN.createDataSet()
    print KNN.classify0([8,0], group, labels, 3)

def datingClassTest():
    hoRatio = 0.10
    datingdataMat, datingLabels = KNN.file2matrix('datingTestSet2.txt')
    normMat, ranges, minVals = KNN.autoNorm(datingdataMat)
    m = normMat.shape[0]
    numTestVecs = int(m*hoRatio)
    errorCount = 0.0
    for i in range(numTestVecs):
        classifierResult = KNN.classify0(normMat[i,:], normMat[numTestVecs:m,:], datingLabels[numTestVecs:m], 3)
        print "the classifier came back with: %d, the real answer is: %d" % (classifierResult, datingLabels[i])
        if (classifierResult != datingLabels[i]):
            errorCount += 1.0
    print "tht total error rate is: %f" % (errorCount/float(numTestVecs))

def handwritingClassTest():
    # 训练
    hwLabels = []
    trainingFileList = listdir('trainingDigits/')
    m = len(trainingFileList)
    trainingMat = zeros((m, 1024))
    for i in range(m):
        fileNameStr = trainingFileList[i]
        fileStr = fileNameStr.split('.')[0]
        classNumStr = int(fileStr.split('_')[0])
        hwLabels.append(classNumStr)
        trainingMat[i,:] = KNN.img2vector('trainingDigits/%s' % fileNameStr)
    
    print "Training done."

    # 测试
    testFileList = listdir('testDigits')
    errorCount = 0.0
    mTest = len(testFileList)
    for i in range(mTest):
        fileNameStr = testFileList[i]
        fileStr = fileNameStr.split('.')[0]
        classNumStr = int(fileStr.split('_')[0])
        vectorUnderTest = KNN.img2vector('testDigits/%s' % fileNameStr)
        classifierResult = KNN.classify0(vectorUnderTest, trainingMat, hwLabels, 3)

        if (classifierResult != classNumStr):
            print "[%s] the classifier came back with: %d, the real answer is: %d" % (fileStr, classifierResult, classNumStr)
            errorCount += 1.0

    print "\nthe total number of errors is: %d" % errorCount
    print "\nthe total error rate is: %f" % (errorCount/float(mTest))

if __name__ == '__main__':
    #test()
    #datingClassTest()
    #KNN.classifyPerson()
    handwritingClassTest()
