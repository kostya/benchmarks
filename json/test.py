import json

text = open('./1.json', 'r').read()
parsed = json.loads(text)
x = 0
y = 0
z = 0
for coord in parsed['coordinates']:
  x += coord['x']
  y += coord['y']
  z += coord['z']

l = len(parsed['coordinates'])
print(x / l)
print(y / l)
print(z / l)