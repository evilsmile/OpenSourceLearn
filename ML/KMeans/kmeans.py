#!/bin/env python
#-*- coding:utf8

from numpy import *
import operator
from math import log


def loadData(fileName):
    dataMat = []
    fr = open(fileName)
    for line in fr.readlines():
        curLine = line.strip().split('\t')
        fltLine = map(float, curLine)
        dataMat.append(fltLine)
    return dataMat

# Calculate the Euclidean distance between two vectors.
def distEclud(vecA, vecB):
    return sqrt(sum(power(vecA - vecB, 2)))

# create a set of k random centroids for a given dataset.
# The random centroids need to be within the bounds of dataset.
def randCent(dataSet, k):
    n = shape(dataSet)[1]
    centroids = mat(zeros((k,n)))
    for j in range(n):
        minJ = min(dataSet[:,j])
        rangeJ = float(max(dataSet[:,j]) - minJ)
        centroids[:,j] = minJ + rangeJ * random.rand(k,1)
    return centroids

# distMeas: distance metric
# createCent: create the inital centroids
def kMeans(dataSet, k, distMeas=distEclud, createCent=randCent):
    m = shape(dataSet)[0]
    clusterAssment = mat(zeros((m,2)))
    centroids = createCent(dataSet, k)
    clusterChanged = True
    # 按照质心->分配->重新计算的过程反复迭代，
    # 直到所有数据点的簇分配结果不再改变为止
    while clusterChanged:
        clusterChanged = False
        for i in range(m):
            minDist = inf; minIndex = -1
            for j in range(k):
                distJI = distMeas(centroids[j,:], dataSet[i,:])
                if distJI < minDist:
                    minDist = distJI; minIndex = j
            if clusterAssment[i,0] != minIndex:
                clusterChanged = True
            clusterAssment[i,:] = minIndex, minDist**2
        print centroids
        # 把簇的均值作为质心
        for cent in range(k):
            ptsInClust = dataSet[nonzero(clusterAssment[:,0].A == cent)[0]]
            # axis=0表示沿着矩阵的列方向计算均值
            centroids[cent,:] = mean(ptsInClust, axis=0)
    return centroids, clusterAssment
