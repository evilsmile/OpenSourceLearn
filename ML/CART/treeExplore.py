#!/bin/env python
#-*- coding:utf8

from Tkinter import *
from numpy import *
import matplotlib
matplotlib.use('TkAgg')
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
import cart

def reDraw(tolS, tolN):
    global reDraw
    reDraw.f.clf()
    reDraw.a = reDraw.f.add_subplot(111)
    global chkBtnVar
    if chkBtnVar.get():
        if tolN < 2:  tolN = 2
        myTree = cart.createTree(reDraw.rawDat, cart.modelLeaf, cart.modelErr, (tolS, tolN))
        yHat = cart.createForeCast(myTree, reDraw.testDat, cart.modelTreeEval)
    else:
        myTree = cart.createTree(reDraw.rawDat, ops=(tolS, tolN))
        yHat = cart.createForeCast(myTree, reDraw.testDat)
    print("reDraw:", reDraw.rawDat)
    reDraw.a.scatter(array(reDraw.rawDat)[:,0], array(reDraw.rawDat[:,1]), s=5)
    reDraw.a.plot(reDraw.testDat, yHat, linewidth=2.0)
    reDraw.canvas.show()

def getInputs():
    global tolNentry
    try:
        tolN = int(tolNentry.get())
    except:
        tolN = 10
        print("Enter integer for tolN")
        tolNentry.delete(0, END)
        tolNentry.insert(0, '10')
    try:
        tolS = int(tolNentry.get())
    except:
        tolS = 10
        print("Enter integer for tolS")
        tolSentry.delete(0, END)
        tolSentry.insert(0, '10')
    return tolN, tolS

def drawNewTree():
    global reDraw
    tolN, tolS = getInputs()
    reDraw(tolS, tolN)

def drawTest():
    global reDraw, tolNentry
    root=Tk()
    reDraw.f = Figure(figsize=(5,4), dpi=100)
    reDraw.canvas = FigureCanvasTkAgg(reDraw.f, master=root)
    reDraw.canvas.show()
    reDraw.canvas.get_tk_widget().grid(row=0, columnspan=3)  
    Label(root, text="tolN").grid(row=1, column=0)
    tolNentry=Entry(root)
    tolNentry.grid(row=1, column=1)
    tolNentry.insert(0, '10')
    Label(root, text="tolS").grid(row=2, column=0)
    tolSentry=Entry(root)
    tolSentry.grid(row=2, column=1)
    tolSentry.insert(0, '1.0')
    Button(root, text="ReDraw", command=drawNewTree).grid(row=1, column=2, rowspan=3)
    
    global chkBtnVar
    chkBtnVar = IntVar()
    chkBtn = Checkbutton(root, text="Model Tree", variable=chkBtnVar)
    chkBtn.grid(row=3, column=0, columnspan=2)
    
    reDraw.rawDat = mat(cart.loadDataSet('sine.txt'))
    reDraw.testDat = arange(min(reDraw.rawDat[:,0]), max(reDraw.rawDat[:,0]), 0.01)
    
    reDraw(1.0, 10)
    root.mainloop()

if __name__ == '__main__':
    drawTest()
