#!/bin/env python
#-*- coding:utf8

import logicReg
from numpy import *

def test():
    dataMat, labelMat = logicReg.loadDataSet()
    print "dataMat:",dataMat
    print "labelMat:",labelMat
    weights = logicReg.gradAscent(dataMat, labelMat)
    #weights = logicReg.stocGradAscent0(dataMat, labelMat)
    weights = logicReg.stocGradAscent1(dataMat, labelMat)
    print "weights:",weights

if __name__ == '__main__':
    test()
