#Importing Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
import json
import csv
#Read the file
df = pd.read_csv (r'C:\Users\chand\Downloads\AnnualTicketSales.csv')
print (df)
print(df.shape)
print(df.columns)
df.shape
df.drop(df.columns[[2]], axis = 1, inplace = True)
df.shape
df.head()
data = pd.get_dummies(df, columns = ['YEAR','TICKETS SOLD'])
data.head()
data.columns
x = data.iloc[:,1:]
x.head()
y = data.iloc[:,0]
y.head()
train, X_test, Y_train, Y_test = train_test_split(x, y, random_state=0)
classifier = LogisticRegression(solver='lbfgs',random_state=0)
classifier.fit(X_train, Y_train)
predicted_y = classifier.predict(X_test)
predicted_y
for x in range(len(predicted_y)):
    if (predicted_y[x] == 0):
        print(x, end="\t")
#Testing the accuracy of the model
print('Accuracy: {:.2f}'.format(classifier.score(X_test, Y_test)))
