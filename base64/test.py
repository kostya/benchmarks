import base64, time
import platform
import socket
import os

STR_SIZE = 131072
TRIES = 8192

str1 = b"a" * STR_SIZE

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))

notify("%s\t%d" % (platform.python_implementation(), os.getpid()))
t, s = time.time(), 0

str2 = base64.b64encode(str1)
print("encode {0}... to {1}...: ".format(str1[:4], str2[:4]), end = '')

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

notify("stop")
