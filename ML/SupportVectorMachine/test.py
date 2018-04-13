#!/bin/env python
#-*- coding:utf8

import svm
from numpy import *

def do_plt(alphas, dataArr):
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

def test():
    dataArr, labelArr = svm.loadDataSet('testSet.txt')
    #b, alphas = svm.smoSimple(dataArr, labelArr, 0.6, 0.001, 40)
    b, alphas = svm.smoP(dataArr, labelArr, 0.6, 0.001, 40)
    #do_plt(alphas, dataArr)

    ws = svm.calcWs(alphas, dataArr, labelArr)
    chk(dataArr, labelArr, ws, b)
   
if __name__ == '__main__':
    test()
