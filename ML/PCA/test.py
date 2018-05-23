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


def test2():
    # secom.data 来自UCI机器学习数据库，包含590个特征， 其中几乎所有样本都存在特征缺失，
    # 用NaN表示，通过replaceNanWithMean将缺失的NaN数据用其它样本的相同特征值平均值填充
    dataMat = pca.replaceNanWithMean('secom.data', ' ') 
    print "dataMat: ", dataMat
    meanVals = mean(dataMat, axis=0)
    meanRemoved = dataMat - meanVals
    covMat = cov(meanRemoved, rowvar=0)
    print "covMat:", covMat
    eigVals,eigVects = linalg.eig(mat(covMat))
    # 有超过20%的特征值都为0，这些特征都是其他特征的副本
    # 前6个特征覆盖了96.8%的方差
    print "eigVals: ", eigVals
    print "eigVects: ", eigVects

if __name__ == '__main__':
    #test()
    test2()
