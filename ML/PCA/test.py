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
    print "dataMat:", dataMat
    print "reconMat:", reconMat
    ax.scatter(dataMat[:,0].flatten().A[0], dataMat[:,1].flatten().A[0], marker='^', s=90)
    ax.scatter(reconMat[:,0].flatten().A[0], reconMat[:,1].flatten().A[0], marker='o', s=50, c='red')
    plt.show()

if __name__ == '__main__':
    test()
