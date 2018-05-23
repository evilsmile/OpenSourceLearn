#!/bin/env python
#-*- coding:utf8

from numpy import *

def loadDataSet(fileName, delim='\t'):
    fr = open(fileName)
    stringArr = [line.strip().split(delim) for line in fr.readlines()]
    datArr =[map(float, line) for line in stringArr]
    return mat(datArr)

def pca(dataMat, topNfeat=999999):
    # calculate the mean of the original dataset
    meanVals = mean(dataMat, axis=0)
    # remove mean
    # 去除平均值 
    meanRemoved = dataMat - meanVals
    # compute the covariance(协方差) matrix 
    # 计算协方差矩阵
    covMat = cov(meanRemoved, rowvar=0)
    # calculate the eigenvalues(特征值)
    # 计算协方差矩阵的特征值和特征向量
    eigVals,eigVects = linalg.eig(mat(covMat))
    # sort top N smallest to largest
    # get the order of the eigenvalues
    # 将特征值从大到小排序，保留对应的最上面的N个特征向量
    eigValInd = argsort(eigVals)
    eigValInd = eigValInd[:-(topNfeat+1):-1]
    # use the order of the eigenvalues to sort the eigenvectors
    # the top N largest eigenvectors form a matrix that will be used
    # to transform our original data into the new space with N features.
    redEigVects = eigVects[:,eigValInd]
    lowDDataMat = meanRemoved * redEigVects
    # Transform data into new dimensions, the reduced dimension dataset.
    # 将数据转换到N个特征向量构建的新空间中
    reconMat = (lowDDataMat * redEigVects.T) + meanVals

    return lowDDataMat, reconMat

def replaceNanWithMean(fileName, delim='\t'):
    datMat = loadDataSet(fileName, delim)
    numFeat = shape(datMat)[1]
    for i in range(numFeat):
        meanVal = mean(datMat[nonzero(~isnan(datMat[:,i].A))[0], i])
        datMat[nonzero(isnan(datMat[:,i].A))[0],i] = meanVal
    return datMat
