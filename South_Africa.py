#!/usr/bin/env python
# coding: utf-8

# In[2]:


import seaborn as sns; 
# Statistical plotting 
import numpy as np
# Array manipulation 
import pandas as pd
# Data manipulation 
import matplotlib.pyplot as plt
# Plotting 
get_ipython().run_line_magic('matplotlib', 'inline')


# In[63]:


# Import the data and check the observations
df = pd.read_csv("dataset.csv")
df.head()


# In[3]:


# Explore the shape of the data
df.shape


# In[32]:


# Import the data and check the observations
df = pd.read_csv("dat_c.csv")
df.head()


# In[33]:


sns.lineplot(x='Year', y='Papers_Published', hue='Research_Types', palette="Accent", data=df)
plt.savefig('papers_published.png', dpi=300)


# In[34]:


# Import the data and check the observations
df = pd.read_csv("mean_c.csv")
df.head()


# In[35]:


sns.lineplot(x='Year', y='Mean_Citations', hue='Research_Types', data=df)
plt.savefig('mean_citations.png', dpi=300)


# In[54]:


# Import the data and check the observations
df = pd.read_csv("model_c.csv")
df.head()


# In[55]:


# Initialize the matplotlib figure

f, ax = plt.subplots(figsize=(6, 10))

sns.barplot(x='Number_of_Studies', y='Model', hue='Research_Types', 
            palette="muted",data=df)
plt.savefig('Model_Studies.png', bbox_inches='tight', pad_inches=0.1, dpi=300)


# In[25]:


# Initialize the matplotlib figure

f, ax = plt.subplots(figsize=(6, 10))

sns.barplot(x='Number_of_Studies', y='Model', hue='Research_Types', 
            palette="rainbow",data=df, order=df.sort_values('Number_of_Studies').Model)


# In[58]:


# Import the data and check the observations
df = pd.read_csv("Tech_c.csv")
df.head()


# In[34]:


# Initialize the matplotlib figure

f, ax = plt.subplots(figsize=(4, 10))

sns.barplot(x='Adv_Methods', y='Number_of_Studies', data=df)
plt.savefig('Tech_Studies.png', bbox_inches='tight', pad_inches=0.1, dpi=300)


# In[61]:


f, ax = plt.subplots(figsize=(4, 10))

sns.barplot(x='Adv_Methods', y='Number_of_Studies',
            palette = "pastel", data=df)
plt.savefig('Tech_Studies_1.png', dpi=300)


# In[ ]:


# Import the data and check the observations
df = pd.read_csv("dataset.csv")
df.head()


# In[66]:


sns.catplot(x='Medicinal_Plants_Usage', y='Means_Citations', kind="violin", 
            palette='muted', data=df)
plt.savefig('Plants_usage_II.png', dpi=300)


# In[67]:


# Import the data and check the observations
df = pd.read_csv("plants_c_II.csv")
df.head()


# In[72]:


f, ax = plt.subplots(figsize=(4, 10))

sns.barplot(x='Research_Types', y='Article_Studying_Medicinal_Plants', 
            palette='muted', data=df)
plt.savefig('Plants_studying.png', dpi=300)


# In[79]:


# Import the data and check the observations
df = pd.read_csv("article_c_I.csv")
df.head()


# In[81]:


sns.barplot(x='Number_of_Articles', y='Plants_Use', 
            palette='muted', data=df)
plt.savefig('Plants_articles_py.png', dpi=300)


# In[76]:


# Import the data and check the observations
df = pd.read_csv("success_c_I.csv")
df.head()


# In[78]:


sns.barplot(x='Number_of_Articles', y='Plants_Success', data=df)
plt.savefig('Plants_success_py.png', dpi=300)


# In[ ]:




