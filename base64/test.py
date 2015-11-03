import base64, time

STR_SIZE = 10000000
TRIES = 100

str1 = b"a" * STR_SIZE
str2 = b""

t, s = time.time(), 0
for _ in range(0, TRIES):
  str2 = base64.b64encode(str1)
  s += len(str2)
print("encode: {0}, {1}".format(s, time.time() - t))

t,   s = time.time(), 0
for _ in range(0, TRIES):
  s += len(base64.b64decode(str2))
print("decode: {0}, {1}".format(s, time.time() - t))
