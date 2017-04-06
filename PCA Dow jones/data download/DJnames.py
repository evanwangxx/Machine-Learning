import urllib.request
import re
import ssl

url = 'https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average'
context = ssl._create_unverified_context()

htmlfile = urllib.request.urlopen(url, context = context)
htmltext = htmlfile.read().decode('utf-8')

regex = '<a rel="nofollow" class="external text" (.+?)</a>'
pattern = re.compile(regex)

name = re.findall(pattern, htmltext)
for i in range(31):
    name[i] = name[i].split('>')[1]

stockname = []
for i in range(30):
    stockname.append(name[i + 1])
    
print(stockname)
print(len(stockname))

f = open("name.csv", "w")
for i in range(len(stockname)):
    f.write(stockname[i] + '\n')
f.close()
    
    
