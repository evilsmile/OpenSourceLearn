#!/bin/env python
#-*- coding:utf8

def predict(w, x):
    return w*x.T

# T和k分别设定了迭代次数和待处理列表的大小。
# 在T次迭代过程中，每次需要重新计算eta.
def batchPegasos(dataSet, labels, lam, T, k):
    import random
    m, n = shape(dataSet); w = zeros(n)
    dataIndex = range(m)
    for t in range(1, T+1):
        wDelta = mat(zeros(n))
        eta = 1.0/(lam*t)
        random.shuffle(dataIndex)
        for j in range(k):
            i = dataIndex[j]
            p = predict(w, dataSet[i,:])
            if labels[i]*p < 1:
                wDelta += labels[i]*dataSet[i,:].A
        w = (1.0-1/t)*w + (eta/k)*wDelta
    return w
