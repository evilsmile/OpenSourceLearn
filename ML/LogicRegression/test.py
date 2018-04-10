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

if __name__ == '__main__':
    test()
