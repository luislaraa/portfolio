#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 2 20:14:02 2022

@author: luislaraa
"""

%reset -f

import os
os.chdir(r"/Users/luislaraa/Desktop/Universidad de Navarra/4to_sem/progra/Proyecto de Progra/archive (4)")

#Here we start by importing all of the tools necessary for the analysis.
import pandas as pd
import numpy as np # linear algebra
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.tsa.stattools as stattools
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler 
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import classification_report, confusion_matrix
import scipy
import statsmodels.formula.api as smf
import statsmodels.api as sm
from sklearn import model_selection, preprocessing, feature_selection, ensemble, linear_model, metrics, decomposition
from lime import lime_tabular
from plotly import tools
import plotly.plotly as py
import plotly.figure_factory as ff
import plotly.graph_objs as go
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
import matplotlib.pyplot as plt
import seaborn as sns
from string import ascii_letters
init_notebook_mode(connected=True)
from scipy.stats import norm
from scipy.stats import skew
from scipy.stats.stats import pearsonr
from scipy import stats
#We import the file
insurance=pd.read_csv("insurance.csv", index_col=0, na_values=r'\N')
#Organize the data and check for NA´s
insurance.fillna(0)
insurance.dropna()

#Basic information
insurance.sort_values(by='charges')
insurance.head
insurance.shape 
insurance.index 
insurance.columns 
insurance.info()
insurance.count() 


descripcion = insurance.describe()

insurance['charges'].describe()    

insurance['charges'].plot(kind='box')

#Division of bmi by categories
insurance['bmi_cat'] = np.nan
lst = [insurance]

for col in lst:
    col.loc[(col["bmi"] >= 0) & (col['bmi'] <= 18.5), 'bmi_cat'] = 'Underweight'
    col.loc[(col["bmi"] > 18.5) & (col['bmi'] <= 25), 'bmi_cat'] = 'Healthy'
    col.loc[(col["bmi"] > 25) & (col['bmi'] <= 30), 'bmi_cat'] = 'Overweight'
    col.loc[(col["bmi"] >= 30) & (col['bmi'] <= 40), 'bmi_cat'] = 'Obese'
    col.loc[(col["bmi"] >= 40) & (col['bmi'] <= 100), 'bmi_cat'] = 'Morbidly Obese'
    
    
    
labels = insurance["bmi_cat"].unique().tolist()
amount = insurance["bmi_cat"].value_counts().tolist()

insurance = insurance.groupby('bmi_cat').size().reset_index(name='counts')
n = insurance['bmi_cat'].unique().__len__()+1
all_colors = list(plt.cm.colors.cnames.keys())
random.seed(100)
c = random.choices(all_colors, k=n)
plt.figure(figsize=(16,10), dpi= 80)
plt.bar(insurance['charges'], insurance['bmi_cat'], color='tab:red', width=.5)
for i, val in enumerate(insurance['counts'].values): 
    plt.text(i, val, float(val), horizontalalignment='center', verticalalignment='bottom', fontdict={'fontweight':500, 'size':12})

plt.plot(insurance['charges'],insurance['bmi_cat'],'.')
insurance['bmi_cat'].value_counts().plot(kind='bar')

#Relationships
plt.plot(insurance['charges'],insurance['bmi'],'.')
plt.plot(insurance['charges'],insurance['region'],'.')
plt.plot(insurance['charges'],insurance['children'],'.')
plt.plot(insurance['charges'],insurance['sex'],'.')
plt.plot(insurance['charges'],insurance['smoker'],'.')


plt.figure(figsize=(12,10), dpi= 80)
sns.heatmap(insurance.corr(), xticklabels=insurance.corr().columns, yticklabels=insurance.corr().columns, cmap='RdYlGn', center=0, annot=True)

#sex as dummy
sex = pd.get_dummies(insurance['sex'],drop_first = True)
df = pd.concat([insurance,sex],axis = 1)
df.info()
smokermean=insurance.groupby(['smoker',]).agg("mean").reset_index()
smokmeancharges=smokermean.loc[:,('smoker','charges')]
plt.figure(figsize=(10, 6))
sns.lmplot(x="bmi", y="charges", hue='smoker', data=insurance, size=5)

regionmean=insurance.groupby(['region',]).agg("mean").reset_index()
regmeanbmi=smokermean.loc[:,('region','bmi')]


#smoker as dummy variable
smoker = pd.get_dummies(insurance['smoker'],drop_first = True)
insurance = pd.concat([insurance,smoker],axis = 1)
insurance.info()
regionmean=insurance.groupby(['region',]).agg("mean").reset_index()
regmeansmoker=regionmean.loc[:,('region','smoker')]

#Filtered top 100
HighCharges=insurance.iloc[0:100,0:15]
plt.figure(figsize=(12,10), dpi= 80)
sns.heatmap(HighCharges.corr(), xticklabels=df.corr().columns, yticklabels=HighCharges.corr().columns, cmap='RdYlGn', center=0, annot=True)


#machine learning for smoker and bmi, distribution and percentages
def utils_recognize_type(insurance, col, max_cat=20): 
    if (insurance[col].dtype == "O") | (insurance[col].nunique() < max_cat):
        return "cat"
    else:
        return "num"
    
dic_cols = {col:utils_recognize_type(insurance, col, max_cat=20) for col in insurance.columns}
heatmap = insurance.isnull()
for k,v in dic_cols.items():
 if v == "num":
   heatmap[k] = heatmap[k].apply(lambda x: 0.5 if x is False else 1)
 else:
   heatmap[k] = heatmap[k].apply(lambda x: 0 if x is False else 1)
sns.heatmap(heatmap, cbar=False).set_title('Dataset Overview')
plt.show()



ax = insurance["smoker"].value_counts().sort_values().plot(kind="barh")
totals= []

for i in ax.patches :
    totals.append(i.get_width())
    
total = sum(totals)

for i in ax.patches :
    ax.text(i.get_width()+.3, i.get_y()+.20, 
    str(round((i.get_width()/total)*100, 2))+'%', 
    fontsize=10, color='black')

ax.grid(axis="x")
plt.suptitle(smoker, fontsize=20)
plt.show()

ax = insurance["bmi"].value_counts().sort_values().plot(kind="barh")
totals= []

for i in ax.patches :
    totals.append(i.get_width())
    
total = sum(totals)

for i in ax.patches :
    ax.text(i.get_width()+.3, i.get_y()+.20, 
    str(round((i.get_width()/total)*100, 2))+'%', 
    fontsize=10, color='black')

ax.grid(axis="x")
plt.suptitle(smoker, fontsize=20)
plt.show()

fig, ax = plt.subplots(nrows=1, ncols=2,  sharex=False, sharey=False)
fig.suptitle("bmi", fontsize=20)

ax[0].title.set_text('distribution')
variable = insurance["bmi"].fillna(insurance["bmi"].mean())
breaks = np.quantile(variable, q=np.linspace(0, 1, 11))
variable = variable[ (variable > breaks[0]) & (variable < 
                    breaks[10]) ]
sns.distplot(variable, hist=True, kde=True, kde_kws={"shade": True}, ax=ax[0])
des = insurance["bmi"].describe()
ax[0].axvline(des["25%"], ls='--')
ax[0].axvline(des["mean"], ls='--')
ax[0].axvline(des["75%"], ls='--')
ax[0].grid(True)
des = round(des, 2).apply(lambda x: str(x))
box = '\n'.join(("min: "+des["min"], "25%: "+des["25%"], "mean: "+des["mean"], "75%: "+des["75%"], "max: "+des["max"]))
ax[0].text(0.95, 0.95, box, transform=ax[0].transAxes, fontsize=10, va='top', ha="right", bbox=dict(boxstyle='round', facecolor='white', alpha=1))
