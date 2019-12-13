import ujson as json
import platform
import socket

def test_json():
    with open('./1.json', 'r') as f:
        jobj = json.loads(f.read())
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

if __name__ == '__main__':
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(platform.python_implementation() + " UltraJSON", 'utf8'))

    test_json()

