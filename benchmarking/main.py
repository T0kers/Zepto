import time


start = time.time()
a = 0
b = 1
for i in range(500):
  c = a + b
  a = b
  b = c
print(c)
print(time.time() - start)
