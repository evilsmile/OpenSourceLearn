#!/bin/env python
#-*- coding:utf8

import bayes
from numpy import *

def simptestTest():
	listOPosts, listClasses = bayes.loadDataSet()
	myVocabList = bayes.createVocabList(listOPosts)
	print myVocabList
	print listOPosts[0]
	print bayes.setOfWords2Vec(myVocabList, listOPosts[0])

def testSimpTrain():
    listOPosts, listClasses = bayes.loadDataSet()
    myVocabList = bayes.createVocabList(listOPosts)
    trainMat = []
    for postinDoc in listOPosts:
        trainMat.append(bayes.setOfWords2Vec(myVocabList, postinDoc))

    print "trainMat:", trainMat
    print "listClasses:", listClasses
    p0V,p1V,pAb = bayes.trainNB0(trainMat, listClasses)
    print "pAb:",pAb
    print "p0V:",p0V
    print "p1V:",p1V

def testingNB():
    listOPosts, listClasses = bayes.loadDataSet()
    myVocabList = bayes.createVocabList(listOPosts)
    trainMat = []
    for postinDoc in listOPosts:
        trainMat.append(bayes.setOfWords2Vec(myVocabList, postinDoc))
    p0V,p1V,pAb = bayes.trainNB0(trainMat, listClasses)

    testEntry = ['love', 'my', 'dalmation', 'stupid']
    thisDoc = array(bayes.setOfWords2Vec(myVocabList, testEntry))
    print testEntry,'classified as: ', bayes.classifyNB(thisDoc, p0V, p1V, pAb)
    testEntry = ['quit', 'stupid']
    thisDoc = array(bayes.setOfWords2Vec(myVocabList, testEntry))
    print testEntry,'classified as: ', bayes.classifyNB(thisDoc, p0V, p1V, pAb)

def textParse(bigString):
    import re
    listOfTokens = re.split(r'\w*', bigString)
    return [tok.lower() for tok in listOfTokens if len(tok) > 2]

def spamTest():
    docList = []; classList = []; fullText = []
    for i in range(1,26):
        wordList = textParse(open('email/spam/%d.txt' % i).read())
        docList.append(wordList)
        fullText.extend(wordList)
        classList.append(1)
        wordList = textParse(open('email/ham/%d.txt' %i).read())
        docList.append(wordList)
        fullText.append(wordList)
        classList.append(0)
    vocabList = bayes.createVocabList(docList)
    trainingSet = range(50); testSet = []
    for i in range(10):
        randIdx = int(random.uniform(0, len(trainingSet)))
        testSet.append(trainingSet[randIdx])
        del(trainingSet[randIdx])

    trainMat = []; trainClasses = []
    for docIndex in trainingSet:
        trainMat.append(bayes.setOfWords2Vec(vocabList, docList[docIndex]))
        trainClasses.append(classList[docIndex])
    p0V,p1V,pSpam = bayes.trainNB0(array(trainMat), array(trainClasses))
    errorCount = 0
    for docIndex in testSet:
        wordVector = bayes.setOfWords2Vec(vocabList, docList[docIndex])
        if bayes.classifyNB(array(wordVector), p0V, p1V, pSpam) != classList[docIndex]:
            errorCount += 1
    print 'the error rate is: ', float(errorCount)/len(testSet)

if __name__ == '__main__':
    #simptestTest()
    #testSimpTrain()
    #testingNB()
    spamTest()
