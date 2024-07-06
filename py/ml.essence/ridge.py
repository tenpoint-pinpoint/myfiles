# リッジ回帰
import numpy as np
from scipy import linalg

class RidgeRegression:
    def __init__(self, lambda_ = 1.):
        self.lambda_ = lambda_
        self.w_ = None
    
    def fit(self, X, y):
        Xtil = np.c_[np.ones(X.shape[0]), X]
        c = np.eye(Xtil.shape[1])                     # np.eye = 単位行列
        A = np.dot(Xtil.T, Xtil) + self.lambda_ * c   # λに単位行列をかけて、行列との掛け算ができるようにしている
        b = np.dot(Xtil.T, y)
        self.w_ = linalg.solve(A, b)                  # デザイン行列を用いているので初めの成分が切片項となる
        
    def predict(self, X):
        Xtil = np.c_[np.ones(X.shape[0]), X]
        return np.dot(Xtil.T, self.w_)