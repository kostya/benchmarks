from __future__ import print_function
import base64, time

STR_SIZE = 131072
TRIES = 8192

str1 = b"a" * STR_SIZE

str2 = base64.b64encode(str1)
print("encode {0}... to {1}...: ".format(str1[:4], str2[:4]), end = '')

t, s = time.time(), 0
for _ in range(0, TRIES):
  str2 = base64.b64encode(str1)
  s += len(str2)
print("{0}, {1}".format(s, time.time() - t))

str3 = base64.b64decode(str2)
print("decode {0}... to {1}...: ".format(str2[:4], str3[:4]), end = '')

t, s = time.time(), 0
for _ in range(0, TRIES):
  str3 = base64.b64decode(str2)
  s += len(str3)
print("{0}, {1}".format(s, time.time() - t))
