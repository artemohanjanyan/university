import random
fout = open("segments.in", "w")
random.seed(None)
n = random.randint(7, 10)
fout.write(str(n) + '\n')
for i in range(n):
    l = list(range(4))
    for i in range(4):
        l[i] = random.randint(-10, 10)
    fout.write('{0} {1} {2} {3}\n'.format(l[0], l[1], l[2], l[3]))
