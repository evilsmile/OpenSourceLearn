#!/bin/env python
#-*- coding:utf8

def loadDataSet():
    return [[1, 3, 4], [2, 3, 5], [1, 2, 3, 5], [2, 5]]

# create a frozenset of each item in C1
# C1 is a candidate itemset of size one
def createC1(dataSet):
    C1 = []
    for transaction in dataSet:
        for item in transaction:
            if not [item] in C1:
                C1.append([item])
    C1.sort()
    return map(frozenset, C1)

# Calculate support for every itemset
# generate L1 from C1
# it returns a dictionary with support values for use later.
def scanD(D, Ck, minSupport):
    ssCnt = {}
    # go over all the transaction
    for tid in D:
        # if the sets of C1 are part of the dataset,
        # increment the count in the dictionary.
        for can in Ck:
            if can.issubset(tid):
                if not ssCnt.has_key(can):
                    ssCnt[can] = 1
                else:
                    ssCnt[can] += 1
    numItems = float(len(D))
    retList= []
    supportData = {}
    for key in ssCnt:
        support = ssCnt[key]/numItems
        # sets that don't meet mininum support levels won't be output
        if support >= minSupport:
            retList.insert(0, key)
        supportData[key] = support
    return retList, supportData


# Create Ck
# take a list of frequent itemsets
def apriorGen(Lk, k): 
    retList = []
    lenLk = len(Lk)
    for i in range(lenLk):
        for j in range(i+1, lenLk):
            # Join set if first k-2 items are equal
            L1 = list(Lk[i])[:k-2]; L2 = list(Lk[j])[:k-2]
            L1.sort(); L2.sort()
            if L1 == L2:
                retList.append(Lk[i] | Lk[j])
    return retList

#  generate a list of candidate itemset
def apriori(dataSet, minSupport = 0.5):
    C1 = createC1(dataSet)
    print "C1:", C1
    D = map(set, dataSet)
    L1, supportData = scanD(D, C1, minSupport)
    L = [L1]
    k = 2
    # scan data set to get Lk from Ck
    while (len(L[k-2]) > 0):
        # call apriorGen() to create candidate itemsets
        Ck = apriorGen(L[k-2], k)
        Lk, supK = scanD(D, Ck, minSupport)
        supportData.update(supK)
        L.append(Lk)
        k += 1
    return L, supportData

# Param:
# @L: a list of frequent itemsets
# @supportData: a dictinary of support data for those items
# @minConf: a minimum confidence threshold
# Function:
# Generate a list of rules with confidence value that we can sort through
def generateRules(L, supportData, minConf=0.7):
    bigRuleList = []
    for i in range(1, len(L)):
        for freqSet in L[i]:
            H1 = [frozenset([item]) for item in freqSet]
            if (i > 1):
                rulesFromConseq(freqSet, H1, supportData, bigRuleList, minConf)
            else:
                calcConf(freqSet, H1, supportData, bigRuleList, minConf)
    return bigRuleList

def calcConf(freqSet, H, supportData, brl, minConf=0.7):
    prunedH = []
    for conseq in H:
        conf = supportData[freqSet]/supportData[freqSet-conseq]
        if conf >= minConf:
            print freqSet-conseq, '-->', conseq, 'conf:', conf
            brl.append((freqSet-conseq, conseq, conf))
            prunedH.append(conseq)
    return prunedH

# try further merging
# create Hm+1 new candidates
def rulesFromConseq(freqSet, H, supportData, brl, minConf=0.7):
    m = len(H[0])
    if (len(freqSet) > (m+1)):
        Hmp1 = apriorGen(H, m+1)
        Hmp1 = calcConf(freqSet, Hmp1, supportData, brl, minConf)
        if (len(Hmp1) > 1):
            rulesFromConseq(freqSet, Hmp1, supportData, brl, minConf)
