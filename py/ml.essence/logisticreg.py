# ロジスティック回帰
import numpy as np
from scipy import linalg

THRESHMIN = 1e-10

def sigmoid(x):
    return 1 / (1 + np.exp(-x))


class LogisticRegression:
    def __init__(self, tol = 0.001, max_iter = 3, random_seed = 0):
        self.tol = tol
        self.max_iter = max_iter
        self.random_state = np.random.RandomState(random_seed) # randomstateはseedを個別に指定できる。seed値がローカル
        self.w_ = None
    
    def fit(self, X, y):
        self.w_ = self.random_state.randn(X.shape[1] + 1)
        Xtil = np.c_[np.ones(X.shape[0]), X] # デザイン行列:切片w0の係数にあたる1列目を生成
        diff = np.inf
        w_prev = self.w_
        iter = 0
        while diff > self.tol and iter < self.max_iter:
            yhat = sigmoid(np.dot(Xtil, self.w_))
            r = np.clip(yhat * (1-yhat), THRESHMIN, np.inf) # THRESHMINを最小値にnp.infを最大値にして配列変更
            XR = Xtil.T * r
            XRX = np.dot(Xtil.T * r, Xtil) # ヘッセ行列
            w_prev = self.w_
            b = np.dot(XR, np.dot(Xtil, self.w_) - 1 / r * (yhat - y)) # self.w_ = w_old,  w_prev = w_new
            self.w_ = linalg.solve(XRX, b)
            diff = abs(w_prev - self.w_).mean()
            iter += 1
            
    def predict(self, X):
        Xtil = np.c_[np.ones(X.shape[0]), X]
        yhat = sigmoid(np.dot(Xtil, self.w_))
        return np.where(yhat > 0.5, 1, 0)    # yhatが0.5以上のところは1、そうでないところは0とする