#!/bin/env python
#-*- coding:utf8

import pca
from numpy import *

def test():
    dataMat = pca.loadDataSet('testSet.txt')
    lowDMat, reconMat = pca.pca(dataMat, 1)
    print "lowDMat: ", shape(lowDMat)

    import matplotlib
    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(dataMat[:,0].flattern().A[0], dataMat[:,1].flattern().A[0], marker='^'

if __name__ == '__main__':
    test()
