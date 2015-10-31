import ujson as json

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
    test_json()

