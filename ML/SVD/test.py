#!/bin/env python
#-*- coding:utf8

import svd
from numpy import *
from numpy import linalg as la

def test():
    Data = svd.loadExData()
    U,Sigma,VT=linalg.svd(Data)
    # Sigma:  [9.72140007e+00 5.29397912e+00 6.84226362e-01 1.76485425e-15 1.07337736e-31]
    # 可以看出前3个数据比最后两个值大了很多，因此可以将最后两个值去掉，
    # 构成一个3X3的对角矩阵Sig3. 
    print "Sigma: ", Sigma
    # 若要从U,Sigma,VT中构造原始矩阵的近似矩阵，只需要用U的前三列和VT的前三行。
    Sig3 = mat([[Sigma[0], 0, 0], [0, Sigma[1], 0], [0, 0, Sigma[2]]])
    # 实际操作中，确定要保留的奇异值的个数有两种方法：
    # 一是将所有的奇异值map成平方和，之后从前向后叠加，直到累加到总值的90%.
    # 二是启发式策略，当矩阵有上万个奇异值时，只保留前3000个，前提是对数据有足够了解，
    #                 确保3000个奇异值足够覆盖总平方和的90%。
    print "Orig: ", U[:,:3]*Sig3*VT[:3,:]


def test2():
    myMat = mat(svd.loadExData())
    print "ecluSim:0, 4 => ", svd.ecluSim(myMat[:,0], myMat[:,4])
    print "ecluSim:0, 0 => ", svd.ecluSim(myMat[:,0], myMat[:,0])
    print "cosSim:0, 4 => ", svd.cosSim(myMat[:,0], myMat[:,4])
    print "cosSim:0, 0 => ", svd.cosSim(myMat[:,0], myMat[:,0])
    print "pearsSim:0, 4 => ", svd.pearsSim(myMat[:,0], myMat[:,4])
    print "pearsSim:0, 0 => ", svd.pearsSim(myMat[:,0], myMat[:,0])

def test3():
    myMat = mat(svd.loadExData())
    # alter a few values
    myMat[0,1]=myMat[0,0]=myMat[1,0]=myMat[2,0]=4
    myMat[3,3]=2
    print "myMatrix:", myMat
    print svd.recommend(myMat, 2)
    print svd.recommend(myMat, 2, simMeas=svd.ecluSim)
    print svd.recommend(myMat, 2, simMeas=svd.pearsSim)

def test4():
    myMat = mat(svd.loadExData2())
    U, Sigma, VT = la.svd(myMat)
    # [15.77075346 11.40670395 11.03044558  4.84639758  3.09292055  2.58097379 1.00413543  0.72817072  0.43800353  0.22082113  0.07367823]
    print Sigma
    Sig2 = Sigma**2
    # 542.0
    print sum(Sig2)
    # 487.8
    print sum(Sig2)*0.9
    # 378.829559511
    print sum(Sig2[:2])
    # 500.500289128
    # 可以发现前3个奇异值就已经占据总能量的90%
    # 因此可以将11维数据缩减为3维
    print sum(Sig2[:3])
    print svd.recommend(myMat, 1, estMethod=svd.svdEst)
    print svd.recommend(myMat, 1, estMethod=svd.svdEst, simMeas=svd.pearsSim)

def test5():
    svd.imgCompress(3)

if __name__ == '__main__':
    #test()
    #test2()
    #test3()
    #test4()
    test5()
