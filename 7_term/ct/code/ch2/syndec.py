import numpy as np
import itertools

h = np.array([
    [0, 1, 0, 1, 0, 0, 1, 1, 1, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 0, 1],
    [0, 1, 1, 1, 1, 1, 0, 0, 1, 1],
    [1, 0, 0, 1, 1, 0, 1, 0, 1, 0],
])

pows = np.ones((h.shape[0]))
cur_pow = 1
for i in range(h.shape[0]):
    pows[i] = cur_pow
    cur_pow *= 2
pows = np.fliplr([pows])[0]

ans = np.ones((2 ** h.shape[0] - 1, h.shape[1]), dtype=np.uint8)

for vec in np.array(list(itertools.product([0, 1], repeat=h.shape[1]))):
    syn = np.dot(vec, h.T) % 2
    if np.sum(syn) != 0:
        i = int(np.sum(syn * pows) - 1)
        prev = ans[i]
        if np.sum(vec) < np.sum(prev):
            ans[i] = vec

print(ans)
