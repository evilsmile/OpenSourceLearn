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
    ax.scatter(xcord3, ycord3, s=60, c='black')
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
                    # finish the optimization, successfully changed a pair of alphas, increment alphaPairsChanged
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
