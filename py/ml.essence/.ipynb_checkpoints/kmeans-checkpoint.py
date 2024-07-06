import numpy as np
import itertools

class KMeans:
    def __init__(self, n_clusters, max_iter = 1000, random_seed = 0):
        self.n_clusters = n_clusters
        self.max_iter = max_iter
        self.random_state = np.random.RandomState(random_seed)
        
    def fit(self, X):
        # まずデータ数分のクラスターラベルを作成する。データ数分、クラスター番号の生成を繰り返す
        cycle = itertools.cycle(range(self.n_clusters))
        self.labels_ = np.fromiter(itertools.islice(cycle, X.shape[0]), dtype=np.int)
        # クラスターラベルをシャッフルする
        self.random_state.shuffle(self.labels_)
        # クラスターラベルを入れる配列を作成する
        labels_prev = np.zeros(X.shape[0]) # １つ前のクラスターを格納する配列を用意
        count = 0
        # クラスタの重心を入れる配列を作る。配列の要素数は[クラスター数, 列数]
        self.cluster_centers_ = np.zeros((self.n_clusters, X.shape[1]))
        # クラスターラベルが完全に入れ替わっていない　and　max_iterに満たない場合続行
        while (not (self.labels_ == labels_prev).all() and count < self.max_iter):
            for i in range(self.n_clusters):
                XX = X[self.labels_ == i, :] # 特定のクラスターラベルのデータ点のみを取り出す
                self.cluster_centers_[i, :] = XX.mean(axis=0) # 取り出したデータ点を利用し、そのクラスターの重心を計算。それを格納
            # 全てのクラスタの重心が再計算されたら重心との距離を計算し、クラスタを振り直す
            dist = ((X[:, :, np.newaxis] - self.cluster_centers_.T[np.newaxis, :, :]) ** 2).sum(axis=1) # 重心とデータ点の距離を計算
            lebels_prev = self.labels_ # 現在のクラスターラベルを1つ前のラベルとして格納
            self.labels_ = dist.argmin(axis=1) # 最も近い重心を選択し、クラスターラベルを更新
            count += 1
            
    def predict(self, X):
        dist = ((X[:,:,np.newaxis] - self.cluster_centers_.T[np.newaxis,:,:])**2).sum(axis=1)
        lebels = dist.argmin(axis=1)
        return labels