#!/bin/env python
#-*- coding:utf8

from numpy import *
import operator
from math import log

def plt(dataMat, classLabels):
    import matplotlib.pyplot as plt
    dataArr = array(dataMat)
    n = shape(dataArr)[0]
    xcord1 = []; ycord1 = []
    xcord2 = []; ycord2 = []
    for i in range(n):
        if (int(classLabels[i]) == 1):
            xcord1.append(dataArr[i, 0])
            ycord1.append(dataArr[i, 1])
        else:
            xcord2.append(dataArr[i, 0])
            ycord2.append(dataArr[i, 1])

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(xcord1, ycord1, s=30, c='red')
    ax.scatter(xcord2, ycord2, s=30, c='green', marker='s')
    plt.xlabel('X1'); plt.ylabel('X2');
    plt.show()


def loadSimpleData():
    datMat = matrix([[1. , 2.1], 
                      [2. , 1.1],
                      [1.3, 1. ],
                      [1. , 1. ],
                      [2. , 1. ]])
    
    classLabels = [1.0, 1.0, -1.0, -1.0, 1.0]
    return datMat, classLabels

