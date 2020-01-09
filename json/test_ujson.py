import ujson as json
import platform
import socket
import os
from pathlib import Path

def test_json(text):
    jobj = json.loads(text)
    l = len(jobj['coordinates'])
    x = 0
    y = 0
    z = 0

    for coord in jobj['coordinates']:
      x += coord['x']
      y += coord['y']
      z += coord['z']

    print(x / l)
    print(y / l)
    print(z / l)

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))

if __name__ == '__main__':
    text = Path('/tmp/1.json').read_text()

    notify("%s UltraJSON\t%d" % (platform.python_implementation(), os.getpid()))

    test_json(text)

    notify("stop")
