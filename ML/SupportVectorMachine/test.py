#!/bin/env python
#-*- coding:utf8

import svm
from numpy import *

def test():
    dataArr, labelArr = svm.loadDataSet('testSet.txt')
    b, alphas = svm.smoSimple(dataArr, labelArr, 0.6, 0.001, 40)
    print "dataArr:",dataArr
    print "labelArr:",labelArr
    print alphas
    print b

if __name__ == '__main__':
    test()
