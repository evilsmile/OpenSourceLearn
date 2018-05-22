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
    meanRemoved = dataMat - meanVals
    # compute the covariance(协方差) matrix 
    covMat = cov(meanRemoved, rowvar=0)
    # calculate the eigenvalues(特征值)
    eigVals,eigVects = linalg.eig(mat(covMat))
    # sort top N smallest to largest
    # get the order of the eigenvalues
    eigValInd = argsort(eigVals)
    eigValInd = eigValInd[:-(topNfeat+1):-1]
    # use the order of the eigenvalues to sort the eigenvectors
    # the top N largest eigenvectors form a matrix that will be used
    # to transform our original data into the new space with N features.
    redEigVects = eigVects[:,eigValInd]
    lowDDataMat = meanRemoved * redEigVects
    # Transform data into new dimensions, the reduced dimension dataset.
    reconMat = (lowDDataMat * redEigVects.T) + meanVals

    return lowDDataMat, reconMat
