#!/bin/env python
#-*- coding:utf8

import svm
from numpy import *

def do_plt(alphas, dataArr, labelArr):
    print(alphas)
    dataA = array(dataArr)

    for i in range(100):
        if alphas[i]>0.0:
            print(dataA[i, 0], dataA[i, 1])
    svm.plotBestFit(alphas, dataArr, labelArr)
 
def chk(dataArr, labelArr, ws, b):
    dataMat = mat(dataArr)
    for idx in range(77):
        print(dataMat[idx] * mat(ws) + b, labelArr[idx])

def testRbf(k1=1.3):
    dataArr, labelArr = svm.loadDataSet('testSetRBF.txt')
    b, alphas = svm.smoP(dataArr, labelArr, 200, 0.0001, 10000, ('rbf', k1))
    datMat = mat(dataArr)
    labelMat = mat(labelArr).transpose()
    svInd = nonzero(alphas.A>0)[0]
    sVs = datMat[svInd]
    labelSV = labelMat[svInd]
    print("there are %d Support Vectors" % shape(sVs)[0])
    errorCount = 0
    m, n = shape(datMat)
    for i in range(m):
        kernelEval = svm.kernelTrans(sVs, datMat[i,:], ('rbf', k1))
        predict = kernelEval.T * multiply(labelSV, alphas[svInd]) + b
        if sign(predict) != sign(labelArr[i]):
            errorCount += 1
    print("the training error rate is: %f" % (float(errorCount)/m))
    dataArr, labelArr = svm.loadDataSet('testSetRBF2.txt')
    errorCount = 0
    datMat = mat(dataArr)
    labelMat = mat(labelArr).transpose()
    m,n = shape(datMat)
    for i in range(m):
        kernelEval = svm.kernelTrans(sVs, datMat[i,:], ('rbf', k1))
        predict = kernelEval.T * multiply(labelSV, alphas[svInd]) + b
        if sign(predict) != sign(labelArr[i]):
            errorCount += 1
    print("the test error rate is: %f" % (float(errorCount)/m))

def test():
    dataArr, labelArr = svm.loadDataSet('testSet.txt')
    #b, alphas = svm.smoSimple(dataArr, labelArr, 0.6, 0.001, 40)
    b, alphas = svm.smoP(dataArr, labelArr, 0.6, 0.001, 40)
    do_plt(alphas, dataArr, labelArr)

    ws = svm.calcWs(alphas, dataArr, labelArr)
    chk(dataArr, labelArr, ws, b)
   
if __name__ == '__main__':
    #test()
    testRbf()
