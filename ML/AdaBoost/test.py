#!/bin/env python
#-*- coding:utf8

import adaboost
from numpy import *

def test():
    datMat, classLabels = adaboost.loadSimpleData()
    print("dataMat: [%s] classLabels: [%s]" % (datMat, classLabels))
    adaboost.plt(datMat, classLabels)
    
if __name__ == '__main__':
    test()
