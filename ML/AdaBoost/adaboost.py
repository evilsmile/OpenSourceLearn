#!/bin/env python
#-*- coding:utf8

from numpy import *
import operator
from math import log

def loadDataSet(fileName):
    numFeat = len(open(fileName).readline().split('\t'))
    dataMat = []; labelMat = []
    fr = open(fileName)
    for line in fr.readlines():
        lineArr = []
        curLine = line.strip().split('\t')
        for i in range(numFeat-1):
            lineArr.append(float(curLine[i]))
        dataMat.append(lineArr)
        labelMat.append(float(curLine[-1]))
    return dataMat, labelMat

def plt(dataMat, classLabels):
    import matplotlib.pyplot as plt
    dataArr = array(dataMat)
    n = shape(dataArr)[0]
    xcord1 = []; ycord1 = []
    xcord2 = []; ycord2 = []
    for i in range(n):
        if (int(classLabels[i]) == 1):
            xcord1.append(dataArr[i, 0])
            ycord1.append(dataArr[i, 1])
        else:
            xcord2.append(dataArr[i, 0])
            ycord2.append(dataArr[i, 1])

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(xcord1, ycord1, s=30, c='red')
    ax.scatter(xcord2, ycord2, s=30, c='green', marker='s')
    plt.xlabel('X1'); plt.ylabel('X2');
    plt.show()

def plotROC(predStrengths, classLabels):
    import matplotlib.pyplot as plt
    cur = (1.0, 1.0)
    ySum = 0.0
    numPosClass = sum(array(classLabels) == 1.0)
    yStep = 1/float(numPosClass)
    xStep = 1/float(len(classLabels)-numPosClass)
    sortedIndicies = predStrengths.argsort()
    fig = plt.figure()
    fig.clf()
    ax = plt.subplot(111)
    for index in sortedIndicies.tolist()[0]:
        if classLabels[index] == 1.0:
            delX = 0; delY = yStep;
        else:
            delX = xStep; delY = 0;
            ySum += cur[1]
        ax.plot([cur[0], cur[0]-delX], [cur[1], cur[1]-delY], c='b')
        cur = (cur[0]-delX, cur[1]-delY)
    ax.plot([0,1], [0,1], 'b--')
    plt.xlabel('False Positive Rate');
    plt.ylabel('True  Positive Rate');
    ax.axis([0,1,0,1])
    plt.show()
    print("the Area Under the Curve is: ", ySum * xStep)

def loadSimpleData():
    datMat = matrix([[1. , 2.1], 
                      [2. , 1.1],
                      [1.3, 1. ],
                      [1. , 1. ],
                      [2. , 1. ]])
    
    classLabels = [1.0, 1.0, -1.0, -1.0, 1.0]
    return datMat, classLabels

# perform a threshold comparison to classify data
def stumpClassify(dataMatrix, dimen, threshVal, threshIneq):
    retArray = ones((shape(dataMatrix)[0], 1))
    if threshIneq == 'lt':
        retArray[dataMatrix[:, dimen] <= threshVal] = -1.0
    else:
        retArray[dataMatrix[:, dimen] > threshVal] = -1.0
    return retArray

# iterate over all of the possible inputs to stumpClassify() 
# and find the best decision stump for our dataset.
# Best here will be with respect to the data weight vector D.
# This function starts out by making sure the input data is 
# in the proper format for matrix math.
def buildStump(dataArr, classLabels, D):
    dataMatrix = mat(dataArr); labelMat = mat(classLabels).T
    m,n = shape(dataMatrix)
    # Be used to iterate over the possible values of the features.
    numSteps = 10.0; 
    # store the classifier information corresponding to the best
    # choice of a decision stump given this weight vector D.
    bestStump = {};
    bestClassEst = mat(zeros((m,1)))
    # init to inifinity.
    # used in finding the mininum possible error.
    minError = inf
    # Goes over all the features in dataset
    for i in range(n):
        rangeMin = dataMatrix[:,i].min(); rangeMax = dataMatrix[:,i].max();
        stepSize = (rangeMax - rangeMin)/numSteps
        for j in range(-1, int(numSteps)+1):
            for inequal in ['lt', 'gt']:
                threshVal = (rangeMin + float(j) * stepSize)
                # return its class prediction based on loop variables.
                predictedVals = stumpClassify(dataMatrix, i, threshVal, inequal)
                # errArr contains a 1 for any value in predictedVals that isn't 
                # equal to the actual class in labelMat.
                errArr = mat(ones((m,1)))
                errArr[predictedVals == labelMat] = 0
                # multiply these errors by the weights in D and sum the results
                # to give a single number weightedError.
                # This is the line where AdaBoost interacts with the classifier
                weightedError = D.T * errArr
                # Calculate weighted error
                print("split: dim %d, thresh %.2f, thresh ineqal: \
                        %s, the weighted error is %.3f" % \
                        (i, threshVal, inequal, weightedError))
                if weightedError < minError:
                    minError = weightedError
                    bestClassEst = predictedVals.copy()
                    bestStump['dim'] = i
                    bestStump['thresh'] = threshVal
                    bestStump['ineq'] = inequal
    return bestStump, minError, bestClassEst

# Decision Stump
def adaBoostTrainDS(dataArr, classLabels, numIt = 40):
    weakClassArr = []
    m = shape(dataArr)[0]
    # D is important, it holds the weight of each piece of data
    # D is a probability distribution, so the sum of all the elements
    # in D must be 1.0. So every element is set to 1/m
    D = mat(ones((m,1))/m)
    # the aggregate estimate of every data point.
    aggClassEst = mat(zeros((m,1)))
    for i in range(numIt):
        # Build a decision stump.
        # returns the stump with the lowest error using D.
        bestStump,error,classEst = buildStump(dataArr, classLabels, D)
        print("D:", D.T)

        # calculate alpha to tell the classifier how much to weight 
        # the output from this stump.
        # max(error, 1e-16) is to make sure there won't be a divided-by-zero
        # error in the case there is no error.
        alpha = float(0.5*log((1.0-error)/max(error, 1e-16)))
        bestStump['alpha'] = alpha
        # it contains all we need for classification.
        weakClassArr.append(bestStump)
        print("ClassEst:", classEst.T)

        expon = multiply(-1*alpha*mat(classLabels).T,classEst)

        # increase the weight of the misclassified pieces of data,
        # and decrease the weight of the properly classified data
        D = multiply(D, exp(expon))
        D = D/D.sum()
        aggClassEst += alpha*classEst
        print("aggClassEst:", aggClassEst.T)
        aggErrors = multiply(sign(aggClassEst) != mat(classLabels).T, ones((m,1)))

        errorRate = aggErrors.sum()/m
        print("total error: ", errorRate, "\n")
        if errorRate == 0.0:
            break

    return weakClassArr

def adaClassify(datToClass, classifierArr):
    # convert to maxtrix
    dataMatrix = mat(datToClass)
    m = shape(dataMatrix)[0]
    aggClassEst = mat(zeros((m,1)))
    for i in range(len(classifierArr)):
        classEst = stumpClassify(dataMatrix, classifierArr[i]['dim'],\
                                 classifierArr[i]['thresh'],\
                                 classifierArr[i]['ineq'])
        aggClassEst += classifierArr[i]['alpha']*classEst
        print("aggClassEst:", aggClassEst)
    return sign(aggClassEst)
