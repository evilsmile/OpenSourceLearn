#!/bin/env python
#-*- coding:utf8

import kmeans
from numpy import *

def test():
    dataMat = mat(kmeans.loadData('testSet.txt'))
    print("min[0]:", min(dataMat[:,0]))
    print("max[0]:", max(dataMat[:,0]))
    print("min[1]:", min(dataMat[:,1]))
    print("max[1]:", max(dataMat[:,1]))

    print("randCent:", kmeans.randCent(dataMat, 2))

    print("distEclud:", kmeans.distEclud(dataMat[0], dataMat[1]))

    myCentroids, clustAssing = kmeans.kMeans(dataMat, 4)
    print("myCentroids:", myCentroids)
    print("clustAssing:", clustAssing)

if __name__ == '__main__':
    test()
