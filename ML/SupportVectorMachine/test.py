#!/bin/env python
#-*- coding:utf8

import svm
from numpy import *

def test():
    dataArr, labelArr = svm.loadDataSet('testSet.txt')
    #b, alphas = svm.smoSimple(dataArr, labelArr, 0.6, 0.001, 40)
    b, alphas = svm.smoP(dataArr, labelArr, 0.6, 0.001, 40)
    print(alphas)
    dataA = array(dataArr)
    for i in range(100):
        if alphas[i]>0.0:
            print(dataA[i, 0], dataA[i, 1])
 #   svm.plotBestFit(alphas, dataArr, labelArr)
    
    #print("dataArr:",dataArr)
    #print("labelArr:",labelArr)
    #print(alphas)
    #print(b)

if __name__ == '__main__':
    test()
