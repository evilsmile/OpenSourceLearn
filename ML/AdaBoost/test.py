#!/bin/env python
#-*- coding:utf8

import adaboost
from numpy import *

def test():
    datMat, classLabels = adaboost.loadSimpleData()
    print("dataMat: [%s] classLabels: [%s]" % (datMat, classLabels))
    #adaboost.plt(datMat, classLabels)
    D = mat(ones((5,1))/5)
    bestStump, minError, bestClassEst = adaboost.buildStump(datMat, classLabels, D) 
    print("bestStump: ", bestStump, " minError:", minError, " bestClasEst:", bestClassEst)

    classifierArray, classifierEst = adaboost.adaBoostTrainDS(datMat, classLabels, 9)
    print("classifierArray:", classifierArray)

    print(adaboost.adaClassify([0,0], classifierArray))
    print(adaboost.adaClassify([[5,5],[0,0]], classifierArray))

def testHolic():
    datArr,labelArr = adaboost.loadDataSet('horseColicTraining2.txt')
    classifierArray, classifierEst = adaboost.adaBoostTrainDS(datArr, labelArr, 10)

    testArr, testLabelArr = adaboost.loadDataSet('horseColicTest2.txt')
    prediction10 = adaboost.adaClassify(testArr, classifierArray)
    print("prediction:", prediction10)

    errArr = mat(ones((67,1)))
    errCnt = errArr[prediction10 != mat(testLabelArr).T].sum()
    print("err count:%d error rate:%.2f" % (errCnt, float(errCnt)/67))

    adaboost.plotROC(classifierEst.T, labelArr)
    
if __name__ == '__main__':
    #test()
    testHolic()
