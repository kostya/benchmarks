import json

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
