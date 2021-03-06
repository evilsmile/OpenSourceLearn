#!/bin/env python
#-*- coding:utf8
# FP - Frequent Pattern

# 生成FP树的事务数据样例
# 事务ID  事务中的元素项
# 001 r, z, h, j, p
# 002 z, y, x, w, v, u, t, s
# 003 z
# 004 r, x, n, o, s
# 005 y, r, x, z, q, t, p
# 006 y, z, x, e, q, s, t, m
def loadSimpleData():
    simplDat = [['r', 'z', 'h', 'j', 'p'],
                ['z', 'y', 'x', 'w', 'v', 'u', 't', 's'],
                ['z'],
                ['r', 'x', 'n', 'o', 's'],
                ['y', 'r', 'x', 'z', 'q', 't', 'p'],
                ['y', 'z', 'x', 'e', 'q', 's', 't', 'm']]
    return simplDat

def createInitSet(dataSet):
    retDict = {}
    for trans in dataSet:
        retDict[frozenset(trans)] = 1
    return retDict


class treeNode:
    def __init__(self, nameValue, numOccur, parentNode):
        self.name = nameValue
        self.count = numOccur
        self.nodeLink = None
        self.parent = parentNode
        self.children = {}

    def inc(self, numOccur):
        self.count += numOccur

    def disp(self, ind=1):
        print ' '*ind, self.name, ' ', self.count
        for child in self.children.values():
            child.disp(ind+1)

# Take dataset and minSup to build a FP-tree.
# This makes two passes through the dataset.
# 1. The first pass goes through everything in the dataset, and counts the freq of each item.
#    These are stored in the header table.
# 2. scan the header table and del items occuring less than 'minSup'.
# 
# The header table is slightly expanded so it can hold a count and pointer to the first item
# of each type.
def createTree(dataSet, minSup=1):
    # 头指针表，存储FP树中元素第一次出现的位置和该元素的总数
    headerTable = {}
    for trans in dataSet:
        for item in trans:
            headerTable[item] = headerTable.get(item, 0) + dataSet[trans]

    # Remove items not meeting min support.
    for k in headerTable.keys():
        if headerTable[k] < minSup:
            del(headerTable[k])

    freqItemSet = set(headerTable.keys())

    # If no items meet min support, exit
    if len(freqItemSet) == 0:
        return None, None

    for k in headerTable:
        headerTable[k] = [headerTable[k], None]

    # Create the base node, which contains the null set.
    retTree = treeNode('Null Set', 1, None)
    for tranSet, count in dataSet.items():
        localD = {}
        for item in tranSet:
            if item in freqItemSet:
                localD[item] = headerTable[item][0]

        # sort transactions by global frequency
        # {'y': 3, 'x': 4, 'r': 3, 't': 3, 'z': 5}
        if len(localD) > 0:
            orderedItems = [v[0] for v in sorted(localD.items(), key=lambda p:p[1], reverse=True)]
            # Populate tree with ordered freq itemset
            updateTree(orderedItems, retTree, headerTable, count)

    return retTree, headerTable

def updateTree(items, inTree, headerTable, count):
    # test if the first item in the transaction exists as a child node
    if items[0] in inTree.children:
        # if so update the count of that item
        inTree.children[items[0]].inc(count)
    else:
        inTree.children[items[0]] = treeNode(items[0], count, inTree)
        # if item not exist create a new treeNode and add it as a child.
        if headerTable[items[0]][1] == None:
            headerTable[items[0]][1] = inTree.children[items[0]]
        else:
            updateHeader(headerTable[items[0]][1], inTree.children[items[0]])

    # Recursively call updateTree on remaining items.
    if len(items) > 1:
        updateTree(items[1::], inTree.children[items[0]], headerTable, count)

def updateHeader(nodeToTest, targetNode):
    while (nodeToTest.nodeLink != None):
        nodeToTest = nodeToTest.nodeLink
    nodeToTest.nodeLink = targetNode

def ascendTree(leafNode, prefixPath):
    if leafNode.parent != None:
        prefixPath.append(leafNode.name)
        ascendTree(leafNode.parent, prefixPath)

def findPrefixPath(basePat, treeNode):
    condPats = {}
    while treeNode != None:
        prefixPath = []
        ascendTree(treeNode, prefixPath)
        if len(prefixPath) > 1:
            condPats[frozenset(prefixPath[1:])] = treeNode.count
        treeNode = treeNode.nodeLink
    return condPats

# mineTree对参数inTree代表的FP树进行频繁项集挖掘。
def mineTree(inTree, headerTable, minSup, preFix, freqItemList):
    # Start from bottom of header table
    # 按元素出现频率从小到大排序
    bigL = [v[0] for v in sorted(headerTable.items(), key=lambda p:p[1])]

    # bigL:  ['y', 's', 't', 'r', 'x', 'z']

    for basePat in bigL:
        newFreqSet = preFix.copy()
        newFreqSet.add(basePat)
        freqItemList.append(newFreqSet)
        condPattBases = findPrefixPath(basePat, headerTable[basePat][1])
        myCondTree, myHead = createTree(condPattBases, minSup)

        if myHead != None:
            # Mine cond. FP-tree
            mineTree(myCondTree, myHead, minSup, newFreqSet, freqItemList)
