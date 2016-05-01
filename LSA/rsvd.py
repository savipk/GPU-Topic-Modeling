import pycuda.gpuarray as gpuarray
import pycuda.autoinit
import numpy as np
from skcuda import linalg, rlinalg
import sys
import timeit
from datetime import datetime
from sklearn.decomposition import RandomizedPCA

linalg.init()
rlinalg.init()

a = np.load(open('/if10/spk3rw/nytimesdata/matrix_'+str(sys.argv[1])+'_docs.bin'))
a = a.astype(np.float32)
a_gpu = gpuarray.to_gpu(a.T)
t = datetime.now()
U, s, Vt = rlinalg.rsvd(a_gpu, k=50, method='standard')
print("GPU Time:")
print(datetime.now()-t)
print(U.shape,s.shape,Vt.shape)

X = a.T
pca = RandomizedPCA(n_components=50)
t = datetime.now()
pca.fit(X)
print("CPU Time:")
print(datetime.now()-t)
print(pca.explained_variance_ratio_)


