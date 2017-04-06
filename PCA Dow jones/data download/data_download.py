# -*- coding: utf-8 -*-

import urllib.request
import DJnames
import ssl

names = DJnames.stockname
context = ssl._create_unverified_context()

f = open("total.csv", "w")
for i in range(0 , len(names)):
    url = "http://ichart.finance.yahoo.com/table.csv?s=" + names[i] + \
               "&a=00&b=1&c=2010&d=00&e=1&f=2011&g=d&ignore=.csv" 

    response = urllib.request.urlopen(url, context = context)
    html = response.read().decode('utf-8')
    non_print = f.write(html)


