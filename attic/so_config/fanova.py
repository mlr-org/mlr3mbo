from fanova import fANOVA
import csv
import numpy as np
X = np.loadtxt("X.csv", delimiter=",")
Y = np.loadtxt("Y.csv", delimiter=",")
f = fANOVA(X,Y)

importances = [None] * X.shape[1]
for i in range(X.shape[1]):
    _, value = f.quantify_importance((i, )).popitem()
    importances[i] = value.get("total importance")
