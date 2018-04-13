#!/bin/env python
#-*- coding:utf8

from numpy import *
import operator
from math import log

def loadDataSet(fileName):
    dataMat = []; labelMat = []
    fr = open(fileName)
    for line in fr.readlines():
        lineArr = line.strip().split('\t')
        dataMat.append([float(lineArr[0]), float(lineArr[1])])
        labelMat.append(float(lineArr[2]))
    return dataMat, labelMat

def selectJrand(i, m):
    j = i
    while (j== i):
        j = int(random.uniform(0, m))
    return j

def clipAlpha(aj, H, L):
    if aj > H:
        aj = H
    if L > aj:
        aj = L
    return aj

def plotBestFit(alpha, dataMat, labelMat):
    import matplotlib.pyplot as plt
    dataArr = array(dataMat)
    n = shape(dataArr)[0]
    xcord1 = []; ycord1 = []
    xcord2 = []; ycord2 = []
    for i in range(n):
        if int(labelMat[i]) == 1:
            xcord1.append(dataArr[i, 0]); 
            ycord1.append(dataArr[i, 1])
        else:
            xcord2.append(dataArr[i, 0]); 
            ycord2.append(dataArr[i, 1])
    xcord3 = []; ycord3 = []
    for i in range(100):
        if alpha[i]>0.0:
            xcord3.append(dataArr[i, 0])
            ycord3.append(dataArr[i, 1])
            
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(xcord1, ycord1, s=30, c='red', marker='s')
    ax.scatter(xcord2, ycord2, s=30, c='green')
    ax.scatter(xcord3, ycord3, s=60, c='black',linewidths=4,alpha=0.2)
#    x = arange(-3.0, 3.0, 0.1)
#    y = (-weights[0] - weights[1]*x)/weights[2]
#    ax.plot(x, y)
    plt.xlabel('X1'); plt.ylabel('X2');
    plt.show()


# b, alphas = svm.smoSimple(dataArr, labelArr, 0.6, 0.001, 40)
# the dataset, the class label, a constant C, the torlance, the maximum number of iterations
def smoSimple(dataMatIn, classLabels, C, toler, maxIter):
    dataMatrix = mat(dataMatIn); 
    labelMat = mat(classLabels).transpose()
    b = 0;
    m,n = shape(dataMatrix)
    # Create an alphas vector filled with 0s
    alphas = mat(zeros((m,1)))
    iter = 0
    # While the number of iterations is less than MaxIterations
    while (iter < maxIter):
        alphaParisChanged = 0
        # For every data vector in the dataset
        for i in range(m):
            # fXi is the prediction of the class
            fXi = float(multiply(alphas, labelMat).T * (dataMatrix*dataMatrix[i,:].T)) + b
            # Ei calculated based on the prediction and the real class of this instance
            # if this error is large, then the alpha corresponding to this data instance can be optimized
            Ei = fXi - float(labelMat[i])
            # if the data vector could be optimized
            # both the positive and negative margins are tested
            if ((labelMat[i] * Ei < -toler) and (alphas[i] < C)) or  \
               ((labelMat[i] * Ei > toler) and (alphas[i] > 0)):
                   # select another data at random
                   j = selectJrand(i, m)
                   # optimize the two vectors together
                   fXj = float(multiply(alphas, labelMat).T * (dataMatrix * dataMatrix[j, :].T)) + b
                   Ej = fXj - float(labelMat[j])
                   # make copy to compare the new alphas and the old ones
                   alphaIold = alphas[i].copy();
                   alphaJold = alphas[j].copy();
                   # Guarantee alphas stay between 0 and C
                   if (labelMat[i] != labelMat[j]):
                       L = max(0, alphas[j] - alphas[i])
                       H = min(C, C + alphas[j] - alphas[i])
                   else:
                       L = max(0, alphas[j] + alphas[i] - C)
                       H = min(C, alphas[j] + alphas[i])
                   if L == H:
                        print("L == H")
                        continue
                    # Eta is the optimal amount to change alpha[j]
                   eta = 2.0 * dataMatrix[i,:] * dataMatrix[j, :].T - dataMatrix[i, :] * dataMatrix[i, :].T - dataMatrix[j, :] * dataMatrix[j, :].T
                   if eta >= 0:
                       print("eta >= 0")
                       continue
                   alphas[j] -= labelMat[j] * (Ei - Ej)/eta
                   # Update i by same amount as j in opposite direction
                   alphas[j] = clipAlpha(alphas[j], H, L)

                   # if alphas[j] has changed by a small amount
                   if (abs(alphas[j] - alphaJold) < 0.00001):
                       print("j not moving enough")
                       continue
                   # alpha[i] is changed by the same amount as alpha[j] in the opossite direction
                   alphas[i] += labelMat[j] * labelMat[i] * (alphaJold - alphas[j])
                   # set the constant term 
                   b1 = b - Ei - labelMat[i] * (alphas[i] - alphaIold) * \
                           dataMatrix[i, :] * dataMatrix[i, :].T -       \
                           labelMat[j] * (alphas[j] - alphaJold) * dataMatrix[i,:] * dataMatrix[j,:].T
                   b2 = b - Ej - labelMat[i] * (alphas[i] - alphaIold) * \
                           dataMatrix[i, :] * dataMatrix[j, :].T - labelMat[j] * (alphas[j] - alphaJold) * \
                           dataMatrix[j,:] * dataMatrix[j,:].T
                   if (0 < alphas[i]) and (C > alphas[i]):
                       b = b1
                   elif (0 < alphas[j]) and (C > alphas[j]):
                       b = b2
                   else:
                       b = (b1+b2)/2.0
                    # finish the optimization, successfully changed a pair of alphas, increment alphaParisChanged
                   alphaParisChanged += 1
                   print("iter: %d i:%d, pairs changed %d" % (iter, i, alphaParisChanged))
            # if no vectors were optimized, increment the iteration count
            # if any alphas have been updated, set iter to 0 and continue
            if (alphaParisChanged == 0):
                   iter += 1
            # nothing changed
            else:
                   iter = 0
            print("iteration number: %d" % iter)
        return b, alphas

# create a data structure to hold all of the important values.
class OptStruct:
    def __init__(self, dataMatIn, classLabels, C, toler):
        self.X = dataMatIn
        self.labelMat = classLabels
        self.C = C
        self.tol = toler
        self.m = shape(dataMatIn)[0]
        self.alphas = mat(zeros((self.m, 1)))
        self.b = 0
        # error cache
        self.eCache = mat(zeros((self.m, 2)))

# calculate an E value for a given alpha and return E values
def calcEk(oS, k):
    fXk = float(multiply(oS.alphas, oS.labelMat).T* \
            (oS.X*oS.X[k, :].T)) + oS.b
    Ek = fXk - float(oS.labelMat[k])
    return Ek

# Inner-loop heuristic
# select the second alpha, or the inner loop alpha
# the goal is to choose the second alpha so that we'll 
# take the maximum step during each optimization.
# it takes the error value associated first choice alpha (Ei)
# and index i.
def selectJ(i, oS, Ei):
    maxK = -1; maxDeltaE = 0; Ej = 0
    # put Ei to valid cache ( valid means it has been calculated )
    oS.eCache[i] = [1, Ei]
    # create a list of nonzero values in the eCache
    validEcacheList = nonzero(oS.eCache[:, 0].A)[0]
    if (len(validEcacheList)) > 1:
        for k in validEcacheList:
            if k == i: continue
            Ek = calcEk(oS, k)
            deltaE = abs(Ei - Ek)
            # Choose j for maximum step size
            if (deltaE > maxDeltaE):
                maxK = k; maxDeltaE = deltaE; Ej = Ek
        return maxK, Ej
    # if first time through the loop, randomly select an alpha
    else:
        j = selectJrand(i, oS.m)
        Ej = calcEk(oS, j)
    return j, Ej

def updateEk(oS, k):
    Ek = calcEk(oS, k)
    oS.eCache[k] = [1, Ek]

def innerL(i, oS):
    Ei = calcEk(oS, i)
    if ((oS.labelMat[i] * Ei < -oS.tol) and (oS.alphas[i] < oS.C)) or \
       ((oS.labelMat[i] * Ei > oS.tol) and (oS.alphas[i] > 0)):
        j, Ej = selectJ(i, oS, Ei)
        alphaIold = oS.alphas[i].copy(); alphaJold = oS.alphas[j].copy()
        if (oS.labelMat[i] != oS.labelMat[j]):
            L = max(0, oS.alphas[j] - oS.alphas[i])
            H = min(oS.C, oS.C + oS.alphas[j] - oS.alphas[i])
        else:
            L = max(0, oS.alphas[j] + oS.alphas[i] - oS.C)
            H = min(oS.C, oS.alphas[j] + oS.alphas[i])

        if L == H: 
            print("L==H"); 
            return 0
        eta = 2.0 * oS.X[i,:] * oS.X[j, :].T - oS.X[i, :] * oS.X[i, :].T - oS.X[j, :] * oS.X[j, :].T
        if eta >= 0: 
            print("eta>=0");
            return 0
        oS.alphas[j] -= oS.labelMat[j]*(Ei-Ej)/eta
        oS.alphas[j] = clipAlpha(oS.alphas[j], H, L)
        updateEk(oS, j)

        if (abs(oS.alphas[j] - alphaJold) < 0.00001):
            print("j not moving enough"); 
            return 0
        oS.alphas[i] += oS.labelMat[j] * oS.labelMat[i] * (alphaJold - oS.alphas[j])
        updateEk(oS, i)
        b1 = oS.b - Ei - oS.labelMat[i] * (oS.alphas[i] - alphaIold) * \
                oS.X[i, :] * oS.X[i, :].T - oS.labelMat[j] * \
                (oS.alphas[j] - alphaJold) * oS.X[i, :]*oS.X[j,:].T

        b2 = oS.b - Ej - oS.labelMat[i] * (oS.alphas[i] - alphaIold) *\
                oS.X[i,:]*oS.X[j,:].T - oS.labelMat[j] * \
                (oS.alphas[j] - alphaJold) * oS.X[j,:] * oS.X[j,:].T
        if (0 < oS.alphas[i]) and (oS.C > oS.alphas[i]): 
            oS.b = b1
        elif (0 < oS.alphas[j]) and (oS.C > oS.alphas[j]): 
            oS.b = b2
        else:  
            oS.b = (b1+b2)/2.0
        return 1
    else:
        return 0

# Much faster compared with smoSimple
def smoP(dataMatIn, classLabels, C, toler, maxIter, kTup=('lin', 0)):
    # create OptStruct to hold all data
    oS = OptStruct(mat(dataMatIn), mat(classLabels).transpose(), C, toler)
    iter = 0
    entireSet = True; alphaParisChanged = 0
    while (iter < maxIter) and ((alphaParisChanged > 0) or (entireSet)):
        alphaParisChanged = 0
        if entireSet:
            for i in range(oS.m):
                alphaParisChanged += innerL(i, oS)
            print("fullSet, iter: %d i:%d, pairs changed %d" % (iter, i, alphaParisChanged))
            iter += 1
        else:
            nonBoundIds = nonzero((oS.alphas.A > 0) * (oS.alphas.A < C))[0]
            for i in nonBoundIds:
                alphaParisChanged += innerL(i, oS)
                print("non-bound, iter: %d i:%d, pairs changed %d" % (iter, i, alphaParisChanged))
            iter += 1
        if entireSet:
            entireSet = False
        elif (alphaParisChanged == 0):
            entireSet = True
        print("iteration number: %d" % iter)
    # 1 will be returned if any pairs get changed
    return oS.b, oS.alphas

def calcWs(alphas, dataArr, classLabels):
    X = mat(dataArr)
    labelMat = mat(classLabels).transpose()
    m,n = shape(X)
    w = zeros((n, 1))
    for i in range(m):
        w += multiply(alphas[i]*labelMat[i], X[i,:].T)
    return w

