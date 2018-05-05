# -*- coding: utf-8 -*-
"""
Created on Mon Apr 23 18:02:22 2018

@author: dillon
"""

from sklearn.decomposition import PCA
from sklearn.preprocessing import normalize
import scipy.io as sio
import matplotlib.image as image
import pandas as pd
import matplotlib.pyplot as plt

#Image is stored in MATLAB dataset
X = sio.loadmat('ex7faces.mat')
X = pd.DataFrame(X['X'])
#Normalize data by subtracting mean and scaling
X_norm = normalize(X)

#Set pca to find principal components that explain 99%
#of the variation in the data
pca = PCA(.99)
#Run PCA on normalized image data
lower_dimension_data = pca.fit_transform(X_norm)
#Lower dimension data is 5000x353 instead of 5000x1024
lower_dimension_data.shape

#Project lower dimension data onto original features
approximation = pca.inverse_transform(lower_dimension_data)
#Approximation is 5000x1024
approximation.shape
#Reshape approximation and X_norm to 5000x32x32 to display images
approximation = approximation.reshape(-1,32,32)
X_norm = X_norm.reshape(-1,32,32)

#Rotate pictures
for i in range(0,X_norm.shape[0]):
    X_norm[i,] = X_norm[i,].T
    approximation[i,] = approximation[i,].T

#Display images
fig4, axarr = plt.subplots(3,2,figsize=(8,8))
axarr[0,0].imshow(X_norm[0,],cmap='gray')
axarr[0,0].set_title('Original Image')
axarr[0,0].axis('off')
axarr[0,1].imshow(approximation[0,],cmap='gray')
axarr[0,1].set_title('99% Variation')
axarr[0,1].axis('off')
axarr[1,0].imshow(X_norm[1,],cmap='gray')
axarr[1,0].set_title('Original Image')
axarr[1,0].axis('off')
axarr[1,1].imshow(approximation[1,],cmap='gray')
axarr[1,1].set_title('99% Variation')
axarr[1,1].axis('off')
axarr[2,0].imshow(X_norm[2,],cmap='gray')
axarr[2,0].set_title('Original Image')
axarr[2,0].axis('off')
axarr[2,1].imshow(approximation[2,],cmap='gray')
axarr[2,1].set_title('99% variation')
axarr[2,1].axis('off')
plt.show()