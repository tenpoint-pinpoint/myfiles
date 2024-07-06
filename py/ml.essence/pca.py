'''
pcaのアルゴリズム
１、共分散行列Sを計算する
２、Sを特異値分解し、S＝UΣVとし、Vの上からc行目を取り出したものをVcとする。ここでVは固有ベクトルとして利用できる
３、与えられたベクトルxに対してVcxを計算する。Vcxは射影後のベクトルとなる
'''

import numpy as np
from scipy.sparse.linalg import svds

class PCA:
    def __init__(self, n_components, tol = 0.0, random_seed = 0):
        self.n_components = n_components
        self.tol = tol
        self.random_state_ = np.random.RandomState(random_seed)
        
    def fit(self, X):
        v0 = self.random_state_.randn(min(X.shape)) # 変数の数はデータ量より少ない前提
        xbar = X.mean(axis=0) # 各変数の平均値を取る
        Y = X - xbar          # x_i - E(x)
        S = np.dot(Y.T, Y)
        U , sigam, VT = svds(S,
                            k = self.n_components, # 特異値と特異ベクトルの数、ここでは固有値の数=圧縮したい次元数を与える
                            tol = self.tol,        # 特異値の許容範囲。0でマシン精度
                            v0 = v0)               # 反復開始ベクトル
        self.VT_ = VT[::-1, :]
        
    def transform(self, X):
        return self.VT_.dot(X.T).T