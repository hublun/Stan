#%matplotlib inline
import pystan
import pickle
import arviz as az
import numpy as np
import multiprocessing

import matplotlib.pyplot as plt

#========== load Stan Model pickled =====================
with open("8school.mdl", 'rb') as f:
    sm = pickle.load(f)
#========================================================
schools_dat = {'J': 8,
               'y': [28,  8, -3,  7, -1,  1, 18, 12],
               'sigma': [15, 10, 16, 11,  9, 11, 10, 18]}
fit = sm.sampling(data = schools_dat, chains=2)
#========================================================
print(fit)
#========================================================
az.style.use('arviz-darkgrid')
az.plot_posterior(np.random.randn(100_000))
az.plot_trace(fit)

az.plot_forest(fit, kind="forestplot", 
    combined=True,
    var_names=['theta'],
    figsize=(9, 7))

plt.show(block=True)