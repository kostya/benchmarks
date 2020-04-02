import json
import platform
import socket
import os
from pathlib import Path

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))


text = Path('/tmp/1.json').read_text()

notify("%s\t%d" % (platform.python_implementation(), os.getpid()))

jobj = json.loads(text)
len = 0
x = 0
y = 0
z = 0

for coord in jobj['coordinates']:
  x += coord['x']
  y += coord['y']
  z += coord['z']
  len += 1

print(x / len)
print(y / len)
print(z / len)

notify("stop")
