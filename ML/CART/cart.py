#!/bin/env python
#-*- coding:utf8

#CART: Classification And Regression Tree
from numpy import *

class treeNode():
    def __init__(self, feat, val, right, left):
        featureToSplitOn = feat
        valueOfSplit = val
        rightBranch = right
        leftBranch = left

def loadDataSet(fileName):
    dataMat = []
    fr = open(fileName)
    for line in fr.readlines():
        curLine = line.strip().split('\t')
        fltLine = map(float, curLine)
        dataMat.append(fltLine)
    return dataMat

def binSplitDataSet(dataSet, feature, value):
    # The two sets are created using array filtering for the given feature and value
#    print("dataSet:", dataSet)
    print("feature:", feature)
    print("value:", value)
    mat0 = dataSet[nonzero(dataSet[:,feature]>value)[0], :]
    mat1 = dataSet[nonzero(dataSet[:,feature]<=value)[0], :]
    return mat0,mat1

def regLeaf(dataSet):
    return mean(dataSet[:,-1])

def regErr(dataSet):
    return var(dataSet[:,-1]) * shape(dataSet)[0]

# leafType: function used to create a leaf
# errType: measuring the rror on the dataset
# ops: a tuple of parameters for creating a tree
def createTree(dataSet, leafType=regLeaf, errType=regErr, ops=(1,4)):
    feat, val = chooseBestSplit(dataSet, leafType, errType, ops)
    if feat == None:
        return val
    retTree = {}
    retTree['spInd'] = feat
    retTree['spVal'] = val
    lSet, rSet = binSplitDataSet(dataSet, feat, val)
    retTree['left'] = createTree(lSet, leafType, errType, ops)
    retTree['right'] = createTree(rSet, leafType, errType, ops)
    return retTree

def chooseBestSplit(dataSet, leafType=regLeaf, errType=regErr, ops=(1,4)):
    tolS = ops[0]; tolN = ops[1]
    # Exit if all values are equal
    if len(set(dataSet[:,-1].T.tolist()[0])) == 1:
        return None, leafType(datSet)

    m,n = shape(dataSet)
    S = errType(dataSet)
    bestS = inf; bestIndx = 0; bestValue = 0
    for featIndx in range(n-1):
        i=0
        for splitVal in set(dataSet[:, featIndx].T.A.tolist()[0]):
            print("---%d" % i)
            i=i+1
            mat0, mat1 = binSplitDataSet(dataSet, featIndx, splitVal)
            print("shape(mat0)[0]:",shape(mat0)[0]," shape(mat1)[0]:",shape(mat1)[0])
            if (shape(mat0)[0] < tolN) or (shape(mat1)[0] < tolN): 
                continue
            newS = errType(mat0) + errType(mat1)
            if newS < bestS:
                bestIndx = featIndx
                bestValue = splitVal
                bestS = newS
    # Exit if low error reduction
    if (S - bestS) < tolS:
        return None, leafType(dataSet)

    # Exit if split creates small dataset
    mat0, mat1 = binSplitDataSet(dataSet, bestIndx, bestValue)
    if (shape(mat0)[0] < tolN) or (shape(mat1)[0] < tolN):
        return None, leafType(dataSet)
    return bestIndx, bestValue
