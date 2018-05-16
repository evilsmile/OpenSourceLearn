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

# The bisecting K-Means clustering
# Give a dataset, the number of clusters, and a distance measure, 
# and it gives the cluster.
# 二分K-均值算法 
# 利用clusterAssment中的误差值评价聚类分簇的正确性和质量.
def biKmeans(dataSet, k, distMeas=distEclud):
    m = shape(dataSet)[0]

    # create a matrix to store the cluster assignment,
    # and squared error for each point in the dataset
    # 存储数据集中每个点的分类结果和平方误差
    clusterAssment = mat(zeros((m, 2)))
    
    # Initially create one cluster
    # calculate one centroid for the entire dataset
    centroid0 = mean(dataSet, axis=0).tolist()[0] 
    # 初始状态所有数据点属于一个大簇,之后每次选择一个簇切分两个簇,
    # 这个切分满足使SSE值最大程度降低,直到簇数目达到k.
    # 保存所有已经划分的簇
    centList = [centroid0]

    # Go over all the point in the dataset and calculate the error
    # between that point and centroid.
    for j in range(m):
        clusterAssment[j, 1] = distMeas(mat(centroid0), dataSet[j, :])**2

    # split cluster until have the desired number of cluster.
    # we can measure the number of clusters we have by measuring
    # the number of items in the cluster list.
    # SSE(误差平方和)评价聚类好坏,值越小表示数据点越接近于它们的质心,聚类效果越好
    # 因为对误差取平方,因此更加重视远离质心的点. 增加簇的个数必然降低SSE的值,但不
    # 符合聚类目标.一种方法是对聚类生成的簇进行后处理,将具有最大SSE值的划分为两个
    # 簇,具体实现只要将属于最大簇的数据点用K-均值聚类,设定簇数k=2即可.
    # 为了保证簇总数不变,可以合并最近的质心,著合并两个使得SSE值增幅最小质心.
    # 循环对簇进行划分,寻找使得SSE值最大程度减小的簇并更新,添加新的簇到centList中
    while (len(centList) < k):
        lowestSSE = inf

        # iterate over all the clusters and find the best cluster to split.
        for i in range(len(centList)):
            # Try spliting every cluster
            ptsInCurrCluster = dataSet[nonzero(clusterAssment[:,0].A==i)[0],:]
            # 在给定的簇上面进行K-均值聚类(k=2)
            centroidMat, splitClustAss = kMeans(ptsInCurrCluster, 2, distMeas)
            # 计算将该簇一分为二后的总误差
            sseSplit = sum(splitClustAss[:,1])
            sseNotSplit = sum(clusterAssment[nonzero(clusterAssment[:,0].A!=i)[0],1])
            print("sseSplit, and notSplit: ", sseSplit, sseNotSplit)
            # Compare SSE after each split
            # if this split produces the lowest SSE, then it's saved.
            if (sseSplit + sseNotSplit) < lowestSSE:
                bestCentToSplit = i
                bestNewCents = centroidMat
                bestClustAss = splitClustAss.copy()
                lowestSSE = sseSplit + sseNotSplit

        # Update the cluster assignment
        bestClustAss[nonzero(bestClustAss[:, 0].A == 1)[0], 0] = len(centList)
        bestClustAss[nonzero(bestClustAss[:, 0].A == 0)[0], 0] = bestCentToSplit
        print("the bestCentToSplit is: ", bestCentToSplit)
        print("the len of bestClustAss is: ", len(bestClustAss))
        centList[bestCentToSplit] = bestNewCents[0,:]
        centList.append(bestNewCents[1,:])
        clusterAssment[nonzero(clusterAssment[:,0].A == bestCentToSplit)[0], :] = bestClustAss

    return array(centList), clusterAssment


import urllib
import json
def geoGrab(stAddress, city):
    apiStem = 'http://where.yahooapis.com/geocode?'
    params = {}
    params['flags'] = 'J'
    params['appid'] = 'aaa0VN6k'
    params['location'] = '%s %s' % (stAddress, city)
    url_params = urllib.urlencode(params)
    yahooApi = apiStem + url_params
    print yahooApi
    c = urllib.urlopen(yahooApi)
    return json.loads(c.read())


from time import sleep
def massPlaceFind(fileName):
    fw = open('places.txt', 'w')
    for line in open(fileName).readlines():
        line = line.strip()
        lineArr = line.split('\t')
        retDict = geoGrab(lineArr[1], lineArr[2])
        if retDict['ResultSet']['Error'] == 0:
            lat = float(retDict['ResultSet']['Result'][0]['latitude'])
            lng = float(retDict['ResultSet']['Result'][0]['longitude'])
            print("%s\t%f\t%f\n" % (line, lat, lng))
        else:
            print("error fetching")
        sleep(1)
    fw.close()


# distance metric for two points on the earth's surface.
def distSLC(vecA, vecB):
    a = sin(vecA[0,1]*pi/180) * sin(vecB[0,1]*pi/180)
    b = cos(vecA[0,1]*pi/180) * cos(vecB[0,1]*pi/180) * cos(pi * (vecB[0,0]-vecA[0,0])/180)

    return arccos(a+b)*6371.0

import matplotlib
import matplotlib.pyplot as plt
# cluster the clubs from a text file and plot them
def clusterClubs(numClust=5):
    datList = []
    for line in open('places.txt').readlines():
        lineArr = line.split('\t')
        datList.append([float(lineArr[4]), float(lineArr[3])])
    datMat = mat(datList)
    myCentroids, clustAssing = biKmeans(datMat, numClust, distMeas=distSLC)

    fig = plt.figure()
    rect = [0.1, 0.1, 0.8, 0.8]
    scatterMarkers = ['s', 'o', '^', '8', 'p', 'd', 'v', 'h', '>', '<']
    axprops = dict(xticks=[], yticks=[])
    ax0 = fig.add_axes(rect, label='ax0', **axprops)
    imgP = plt.imread('Portland.png')
    ax0.imshow(imgP)
    ax1 = fig.add_axes(rect, label='ax1', frameon=False)
    for i in range(numClust):
        ptsInCurrCluster = datMat[nonzero(clustAssing[:,0].A==i)[0], :]
        markerStyle = scatterMarkers[i % len(scatterMarkers)]
        ax1.scatter(ptsInCurrCluster[:,0].flatten().A[0], \
                    ptsInCurrCluster[:,1].flatten().A[0], \
                    marker=markerStyle, s=90)
    ax1.scatter(mat(myCentroids)[:,0].flatten().A[0], \
                mat(myCentroids)[:,1].flatten().A[0], marker='+', s=300)
    plt.show()
