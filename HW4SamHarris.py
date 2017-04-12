import pandas as pd
import math
    
def GenerateTree(initialList, origarray, origarrayrows):
    currList = initialList
    
    stoppingRule = 0
    
    while stoppingRule == 0:
        newList = []
        for i in range(0,len(currList),1):
            if currList[i][-1] != 0:
                abbrarray = RemoveCols(origarray, currList[i][-2])
                arraysub = SubsetArray(origarrayrows, currList[i])
                arraysub = RemoveCols(arraysub, currList[i][-2])
                best = GetBest(arraysub,currList[i][-1])
                extractedCol = abbrarray[best[0]]
                for k in range(0,len(origarray)-1,1):
                    if extractedCol == origarray[k]:
                        appending = [k]
                
                setarr = set(abbrarray[best[0]])
                setlist = list(setarr)
                for j in range(0, len(setlist),1):
                    intlist = currList[i][:-2]
                    intlist.append(setlist[j])
                    newlist = currList[i][-2]+appending
                    intlist.append(newlist)
                    intlist2 = AppendEntropy(intlist, origarrayrows)
                    newList.append(intlist2)
                    
            else:
                newList.append(currList[i])
            
        currList = newList    
        
        checkList = [item[-1] for item in currList] 
        stoppingRule = 1
        for i in range(0,len(checkList),1):
            if checkList[i] != 0:
                stoppingRule = 0
        
    return currList
    
def RemoveCols(original, removing):
    removelist = [i for j, i in enumerate(original) if j not in removing]
    return removelist

def AppendEntropy(listin, origarrayrows):
    
    listinorig = listin
    listinnew = listin[:-1]
    listinnew.pop(0)
    
    initial = []

    init = [0,0]

    for i in range(0,len(origarrayrows),1):
        if set(listinnew).issubset(set(origarrayrows[i])) and origarrayrows[i][-1] == 'Yes':
            init = [x+y for x,y in zip(init,[1,0])]
        if set(listinnew).issubset(set(origarrayrows[i])) and origarrayrows[i][-1] == 'No':
            init = [x+y for x,y in zip(init,[0,1])]
        
    initial.append(init)
         
    entropy = CalcEntropy(initial)

    if entropy == 0 and initial[0][0] > 0:
        listinorig.append("Yes")
    if entropy == 0 and initial[0][1] > 0:
        listinorig.append("No")
    
    listinorig.append(entropy)
    
    return listinorig

def SubsetArray(origarrayrows, currentlist):
    
    listin = currentlist[:-2]
    listin.pop(0)
    
    vals = []
    
    for i in range(0,len(origarrayrows),1):
        if set(listin).issubset(set(origarrayrows[i])):
            vals.append(origarrayrows[i])
            
    vals = list(map(list, zip(*vals)))
    return vals
    
def GetBest(array, entropy):
    
    bestEntr = [0,entropy]
    
    for i in range(0,len(array)-1, 1):
        listbest = getList(i,array)
        entropy = CalcEntropy(listbest)
        if entropy < bestEntr[1]:
            bestEntr = [i, entropy]
        
    return bestEntr
        
def GetInitial(vals):

    initial = []

    init = [0,0]

    for i in range(0,len(vals[1]),1):
        if vals[len(vals)-1][i] == 'Yes':
            init = [x+y for x,y in zip(init,[1,0])]
        else:
            init = [x+y for x,y in zip(init,[0,1])]
        
    initial.append(init)

    return initial


def getList(index, data):
    setarr = set(data[index])
    setlist = list(setarr)
    
    init = []
    
    for i in range(0,len(setlist),1):
        
        init.append([0,0])
            
        for j in range(0,len(data[len(data)-1]),1):
            if data[index][j] == setlist[i] and data[len(data)-1][j] == 'Yes':
                init[i] = [x+y for x,y in zip(init[i],[1,0])]
            if data[index][j] == setlist[i] and data[len(data)-1][j] == 'No':
                init[i] = [x+y for x,y in zip(init[i],[0,1])]

    return init

def RemoveEleTree(tree):
    
    for x in tree:
        del x[0]
        
        x[-3], x[-2] = x[-2], x[-3]
    
    return tree

def RemoveLast(tree):
    
    for x in tree:
        del x[-1]
        del x[-1]
    
    return tree

def CalcEntropy(listin):
    sumL = 0
    for i in range(0,len(listin),1):
        sumL = sumL + sum(listin[i])
        
    entropy = 0
    for i in range(0,len(listin),1):
        elements = sum(listin[i])
        entropy = entropy + (elements / sumL)*CalcEntropySub(listin[i], elements)
        
    return entropy

def CalcEntropySub(listele, elements):
    
    if listele[0] == 0 or listele[1] == 0:
        entropy = 0
    else:
        entropy = -(listele[0]/elements)*math.log2(listele[0]/elements)-((listele[1]/elements)*math.log2(listele[1]/elements))
    
    return entropy

data = pd.read_csv('tennis.csv', header=None)
vals = data.values.T.tolist()
valsrows = data.values.tolist()
origarray = vals
origarrayrows = valsrows

initial = GetInitial(vals)
assentr = CalcEntropy(initial)
    
initialList = [["Initial", [], assentr]]
    
tree = GenerateTree(initialList, origarray, origarrayrows)
    
tree = RemoveEleTree(tree)
    
print(tree)

tree = RemoveLast(tree)

print(tree)



        

    
    