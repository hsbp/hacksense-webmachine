#!/usr/bin/env python
# -*- coding: utf-8 -*-

from lxml import etree
import requests
import os

resp = requests.post(u'http://openhort.ártó.hu/arduino_data/displayData.php',
        {'table': 'sensors', 'from': '', 'to': '', 'sensorslength': '1'})
tree = etree.fromstring(resp.content, etree.HTMLParser())
temps = tree.xpath('//div[@id="sensorstable"]/table/tr[position() > 1]/td[position() > 2]/text()')
txt = os.path.join(os.path.dirname(__file__), 'openhort.txt')
tmp = txt + '.tmp'

with file(tmp, 'w') as f:
    f.write('\n'.join(temps))

os.rename(tmp, txt)
