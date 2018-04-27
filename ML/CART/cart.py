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

# 通过数组过滤切分的数据集
def binSplitDataSet(dataSet, feature, value):
    # The two sets are created using array filtering for the given feature and value
#    print("dataSet:", dataSet)
#    print("feature:", feature)
#    print("value:", value)
    mat0 = dataSet[nonzero(dataSet[:,feature]>value)[0], :]
    mat1 = dataSet[nonzero(dataSet[:,feature]<=value)[0], :]
    return mat0,mat1

def regLeaf(dataSet):
    return mean(dataSet[:,-1])

def regErr(dataSet):
    return var(dataSet[:,-1]) * shape(dataSet)[0]

# leafType: function used to create a leaf
# errType: measuring the error on the dataset
# ops: a tuple of parameters for creating a tree
def createTree(dataSet, leafType=regLeaf, errType=regErr, ops=(1,4)):
    # 找到最佳的待切分特征
    feat, val = chooseBestSplit(dataSet, leafType, errType, ops)
    if feat == None:
        return val
    retTree = {}
    retTree['spInd'] = feat
    retTree['spVal'] = val
    # 执行二元切分
    lSet, rSet = binSplitDataSet(dataSet, feat, val)
    # 左右子树递归
    retTree['left'] = createTree(lSet, leafType, errType, ops)
    retTree['right'] = createTree(rSet, leafType, errType, ops)
    return retTree

# 目标是找到数据切分的最佳位置
# 遍历所有的特征及其可能的取值来找到使误差最小化的划分阈值。
# leafType用来生成叶节点，在回归树中该模型是目标变量的均值。
# errType是误差估计函数，计算目标变量的总方差.
def chooseBestSplit(dataSet, leafType=regLeaf, errType=regErr, ops=(1,4)):
    # tolS是容许的误差下降值
    # tolN是切分的最小样本数
    tolS = ops[0]; tolN = ops[1]
    # Exit if all values are equal
    # 剩余特征值都相同
    if len(set(dataSet[:,-1].T.tolist()[0])) == 1:
        return None, leafType(dataSet)

    m,n = shape(dataSet)
    # 计算误差
    S = errType(dataSet)
    bestS = inf; bestIndx = 0; bestValue = 0
    for featIndx in range(n-1):
        i=0
        for splitVal in set(dataSet[:, featIndx].T.A.tolist()[0]):
            i=i+1
            mat0, mat1 = binSplitDataSet(dataSet, featIndx, splitVal)
            #print("shape(mat0)[0]:",shape(mat0)[0]," shape(mat1)[0]:",shape(mat1)[0])
            # 切分子集数目小
            if (shape(mat0)[0] < tolN) or (shape(mat1)[0] < tolN): 
                continue
            newS = errType(mat0) + errType(mat1)
            if newS < bestS:
                bestIndx = featIndx
                bestValue = splitVal
                bestS = newS
    # Exit if low error reduction
    # 误差下降不够大
    if (S - bestS) < tolS:
        return None, leafType(dataSet)

    # Exit if split creates small dataset
    mat0, mat1 = binSplitDataSet(dataSet, bestIndx, bestValue)
    if (shape(mat0)[0] < tolN) or (shape(mat1)[0] < tolN):
        return None, leafType(dataSet)
    return bestIndx, bestValue

def isTree(obj):
    return (type(obj).__name__ == 'dict')

def getMean(tree):
    if isTree(tree['right']):
        tree['right'] = getMean(tree['right'])
    if isTree(tree['left']):
        tree['left'] = getMean(tree['left'])
    return (tree['left'] + tree['right'])/2.0

# 树构建算法对输入的tolS和tolN非常敏感，将ops换为(0,1)会发现生成的树非常臃肿，几乎为数据集中的
# 每个样本都分配了一个叶节点。加载ex2.txt的数据，该数据集和前面的ex00.txt的数据分布类似，但数量
# 级是后者的100倍，而ex00构建出的树只有两个叶节点。原因在于停止条件tolS对误差的数量级非常敏感。
# 如果树节点过多，则该模型可能对数据过拟合，通过降低决策树的复杂度来避免过拟合的过程称为剪枝。
# 上面函数的chooseBestSplit中的三个提前终止条件是“预剪枝”操作，另一种形式的剪枝老板娘使用
# 测试集和训练集，称为“后剪枝”。
# 使用后剪枝方法需要将数据集交叉验证，首先给定参数，使得构建出的树足够复杂，之后从上而下找
# 到叶节点，判断两个叶节点是否能够取得更好的测试误差，如果是就合并。
# tree: tree to prune
# testData: use for pruning the tree 
def prune(tree, testData):
    # check if empty
    if shape(testData)[0] == 0:
        return getMean(tree)

    if (isTree(tree['right']) or isTree(tree['left'])):
        lSet, rSet = binSplitDataSet(testData, tree['spInd'], tree['spVal'])

    if isTree(tree['left']):
        tree['left'] = prune(tree['left'], lSet)

    if isTree(tree['right']):
        tree['right'] = prune(tree['right'], rSet)

    if not isTree(tree['left']) and not isTree(tree['right']):
        lSet, rSet = binSplitDataSet(testData, tree['spInd'], tree['spVal'])
        errorNoMerge = sum(power(lSet[:,-1] - tree['left'], 2)) + \
                       sum(power(rSet[:,-1] - tree['right'], 2))
        treeMean = (tree['left'] + tree['right'])/2.0
        errorMerge = sum(power(testData[:,-1] - treeMean, 2))
        if errorMerge < errorNoMerge:
            print("Mergeing")
            return treeMean
        else:
            return tree
    else:
        return tree

# Format the dataset into the target variable Y and independent variable X.
# X and Y are used to perform a simple linear regression.
def linearSolve(dataSet):
    m,n = shape(dataSet)
    X = mat(ones((m,n))); Y = mat(ones((m,1)))
    X[:,1:n] = dataSet[:,0:n-1]; Y = dataSet[:,-1]
    xTx = X.T*X
    if linalg.det(xTx) == 00:
        raise NameError("This matrix is singular, cannot do inverse, \ntry increasing the second value of ops")
    wx = xTx.I * (X.T *Y)
    return wx, X, Y

# Generate a model for a leaf node once you've determined to no longer split the data.
def modelLeaf(dataSet):
    ws, X, Y = linearSolve(dataSet)
    return ws

# Compute the error for a given dataset.
def modelErr(dataSet):
    ws, X, Y = linearSolve(dataSet)
    yHat = X * ws
    return sum(power(Y - yHat, 2))

def regTreeEval(model, inDat):
    return float(model)

def modelTreeEval(model, inDat):
    n = shape(inDat)[1]
    X = mat(ones((1,n+1)))
    X[:,1:n+1] = inDat
    return float(X*model)

# take a single data point or row vector and return a single floating-point value.
# 对于输入的单个数据点，返回一个预测值。
# modelEval是对叶节点数据进行预测的函数的引用，函数treeForeCast自顶向下遍历整棵树，
# 直到命中叶节点为止。一旦到达叶节点，它会在输入数据上调用modelEval，该参数默认值
# 是regTreeEval。要对回归树叶节点预测，就调用regTreeEval,要对模型树节点预测，调用
# modelTreeEval.
def treeForeCast(tree, inData, modelEval=regTreeEval):
    if not isTree(tree):
        return modelEval(tree, inData)
    if inData[tree['spInd']] > tree['spVal']:
        if isTree(tree['left']):
            return treeForeCast(tree['left'], inData, modelEval)
        else:
            return modelEval(tree['left'], inData)
    else:
        if isTree(tree['right']):
            return treeForeCast(tree['right'], inData, modelEval)
        else:
            return modelEval(tree['right'], inData)

# 模型树将叶节点设置为分段线性函数，即模型由多个线性片段组成。决策树相比其他机器学习算法
# 易于理解，而模型树的可解释性是它优于回归的特性之一。模型树同时具备更高的预测准确度。
def createForeCast(tree, testData, modelEval=regTreeEval):
    m=len(testData)
    yHat = mat(zeros((m,1)))
    for i in range(m):
        yHat[i,0] = treeForeCast(tree, mat(testData[i]), modelEval)
    return yHat

