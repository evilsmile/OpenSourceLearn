#!/bin/env python
#-*- coding:utf8

# singular value decomposition
# 奇异值
from numpy import *
from numpy import linalg as la

def loadExData():
    return [[1,1,1,0,0],
            [2,2,2,0,0],
            [1,1,1,0,0],
            [5,5,5,0,0],
            [1,1,0,2,2],
            [0,0,0,3,3],
            [0,0,0,1,1]]

def loadExData2():
    return[[0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 5],
           [0, 0, 0, 3, 0, 4, 0, 0, 0, 0, 3],
           [0, 0, 0, 0, 4, 0, 0, 1, 0, 4, 0],
           [3, 3, 4, 0, 0, 0, 0, 2, 2, 0, 0],
           [5, 4, 5, 0, 0, 0, 0, 5, 5, 0, 0],
           [0, 0, 0, 0, 5, 0, 1, 0, 0, 5, 0],
           [4, 3, 4, 0, 0, 0, 0, 5, 5, 0, 1],
           [0, 0, 0, 4, 0, 4, 0, 0, 0, 0, 4],
           [0, 0, 0, 2, 0, 2, 5, 0, 0, 1, 2],
           [0, 0, 0, 0, 5, 0, 0, 0, 0, 4, 0],
           [1, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0]]

# 欧式距离：
def ecluSim(inA, inB):
    return 1.0/(1.0 + la.norm(inA - inB))

# 皮尔逊相关系数：优于欧式距离的地方是它对用户评分的量级不敏感，
# 如所有评分都是5分和1分在这里是相同 的。
def pearsSim(inA, inB):
    if len(inA) < 3 :
        return 1.0
    # corrcoef()方法用于计算皮尔逊相关系数。
    # 控制相似度在0~1之间
    return 0.5+0.5*corrcoef(inA, inB, rowvar=0)[0][1]

# 余弦相似度：对于两个向量，计算其夹角余弦值来比较相似度。
def cosSim(inA, inB):
    num = float(inA.T*inB)
    # norm()用于计算单个向量的2范数（平方和取根）。
    demon = la.norm(inA)*la.norm(inB)
    # 因为cos在-1~1之间，同样用0.5+0.5*cos来控制相似度在0~1之间
    return 0.5+0.5*(num/demon)

# calculate the estimated rating a user would give an item for a given similarity measure.
# 通常用于推荐引擎评价的指标是"最小方均根误差(RMSE)"，它计算均方误差的平均值并开根。
# 若评级在1~5分，而RMSE的结果为1.0，说明预测值和用户给出的真实评价差了一分。
# 该函数用于在给定计算相似度方法的前提下，计算用户对某种物品的可能评分。
def standEst(dataMat, user, simMeas, item):
    n = shape(dataMat)[1]
    simTotal = 0.0; ratSimTotal = 0.0
    # 假设要计算用户对其未打分的菜肴i的可能评分,需要通过其他物品j和物品i建立联系
    # 扫描所有N个物品，如果用户u对某个物品j有过评分，则寻找所有用户中既对i又对j
    # 评分过的用户群体users，根据users们的打分,计算出物品i和物品j的相似度
    # 最后将这个相似度乘以用户u对物品j的评分累加到ratSimTotal，将相似度累加到
    # simTotal变量。最后返回ratSimTotal/simTotal就是可能评分。
    for j in range(n):
        userRating = dataMat[user, j]
        if userRating == 0:
            continue
        # overLap captures the elements that have been rated between two items.
        overLap = nonzero(logical_and(dataMat[:,item].A>0, dataMat[:,j].A>0))[0]
        if len(overLap) == 0:
            similarity = 0
        else:
            similarity = simMeas(dataMat[overLap, item], dataMat[overLap, j])
        print 'the %d and %d similarity is: %f' % (item, j, similarity)
        simTotal += similarity
        ratSimTotal += similarity * userRating
    if simTotal == 0:
        return 0
    else:
        # normalize the similarity rating product by dividing it by the sum of all ratings.
        # This will give a number between 0 and 5, which used to rank the forecarsted values.
        return ratSimTotal/simTotal

# Used to replace standEst() when call recommend().
# This function creates an estimated rating for a given item for 
# a given user.
def svdEst(dataMat, user, simMeas, item):
    n = shape(dataMat)[1]
    simTotal = 0.0; ratSimTotal = 0.0
    # compared to standEst, this calls svd()
    # You use only the singular values that give you 90% of the energy after SVD.
    U, Sigma, VT = la.svd(dataMat)
    # The singular values are given in form of a NumPy array
    # Build a diagonal matrix with these singular values on the diagonal.
    # 自动找出能量大于90%的特征值个数
    sumTotal = 0
    for singular in Sigma:
        sumTotal += singular**2
    singularMax = 0
    for i in range(len(Sigma)):
        singularMax += Sigma[i]**2
        if singularMax >= sumTotal*0.9:
            break
    SigI = mat(eye(i+1)*Sigma[:i+1])

    # Use the U matrix to transform our items into the lower-dimensional space.
    # 评估相似度时使用的是一个NxI的矩阵, I是提取的奇异值个数, N是物品数目
    #    NxI     =     NxM       MxI        IxI
    # 不同用户对物品k的评分已经被压缩到第k行的i个数据中，只要计算两个1xI向量的相似度即可
    xformedItems = dataMat.T * U[:,:i+1] * SigI.I
    # iterate over all the elements in a row for a given user.
    # This servers the same purpose as the for loop in standEst() except
    # the similarities are calculated in a lower dimension.
    for j in range(n):
        userRating = dataMat[user, j]
        if userRating == 0 or j == item:
            continue
        similarity = simMeas(xformedItems[item,:].T, xformedItems[j,:].T)
        print 'the %d and %d similarity is: %f' % (item, j, similarity)
        simTotal += similarity
        ratSimTotal += similarity * userRating
    if simTotal == 0:
        return 0
    else:
        return ratSimTotal/simTotal

# Recommendation Engine.
# 
def recommend(dataMat, user, N=3, simMeas=cosSim, estMethod=standEst):
    unratedItems= nonzero(dataMat[user,:].A==0)[1]
    if len(unratedItems) == 0:
        return 'you rated everything'
    itemScores = []
    for item in unratedItems:
        estimatedScore = estMethod(dataMat, user, simMeas, item)
        itemScores.append((item, estimatedScore))
    return sorted(itemScores, key=lambda jj:jj[1], reverse=True)[:N]


def printMat(inMat, thresh=0.8):
    for i in range(32):
        for k in range(32):
            if float(inMat[i, k]) > thresh:
                print 1,
            else:
                print 0,
        print ''

# 用一个32*32（1024像素）的矩阵表示一个图像，通过svd对该矩阵降维，实现压缩重构。
# 基本还原了原来的矩阵。 U和VT都是32X2的矩阵，有两个奇异值，因此总数字数目为
# 62*2+2=130，获得了几乎10倍的压缩比。
def imgCompress(numSV=3, thresh=0.8):
    my1 = []
    for line in open('0_5.txt').readlines():
        newRow = []
        for i in range(32):
            newRow.append(int(line[i]))
        my1.append(newRow)
    myMat = mat(my1)
    print "*****original matrix*******"
    printMat(myMat, thresh)
    U,Sigma,VT = la.svd(myMat)
    SigRecon = mat(zeros((numSV, numSV)))
    for k in range(numSV):
        SigRecon[k,k] = Sigma[k]
    reconMat = U[:,:numSV] * SigRecon * VT[:numSV,:]
    print "*****reconstructed matrix using %d singular values*****" % numSV
    printMat(reconMat, thresh)
