import json
import platform
import socket

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    if not s.connect_ex(("localhost", 9001)):
        s.sendall(bytes(platform.python_implementation(), 'utf8'))

text = open('./1.json', 'r').read()
jobj = json.loads(text)
len = len(jobj['coordinates'])
x = 0
y = 0
z = 0

for coord in jobj['coordinates']:
  x += coord['x']
  y += coord['y']
  z += coord['z']

print(x / len)
print(y / len)
print(z / len)
