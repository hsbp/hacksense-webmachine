#!/usr/bin/env python

from os import path, rename
import requests, json

with file(path.join(path.dirname(__file__), 'waterlevel.json')) as f:
	cfg = json.load(f)

resp = requests.post('https://api.spark.io/v1/devices/{0}/measure'.format(cfg['device_id']),
	{'access_token': str(cfg['access_token'])})

txt = path.join(path.dirname(__file__), 'waterlevel.txt')
tmp = txt + '.tmp'

with file(tmp, 'w') as f:
    f.write(str(resp.json()['return_value']))

rename(tmp, txt)
