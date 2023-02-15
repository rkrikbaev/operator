import requests
import time
import json
from datetime import datetime, timedelta, timezone
import csv
import os


def request(payload, url)->dict:

    with requests.Session() as s:
        data=json.dumps(payload)
        _ = s.post(
                url,
                headers={'Content-Type': 'application/json'},
                data=data,
                timeout=600).json()

def read_and_send( data, base, batch=10):

    ts_today = [ int((base + timedelta(seconds=x)).timestamp()) for x in range(1, len(data), 1) ]
    dataset = [ {"ts":ts*1000, "value":float(x[1])} for ts, x in zip(ts_today, data) ]
    i = 10
    
    while True:

        ts = int(datetime.now().timestamp())*1000
        try: 
            while ts >= dataset[i]["ts"]:

                payload = dataset[i-batch:i]

                request(payload, url = 'http://165.22.78.114:9000/rest/server/simaction')

                i +=10

                print(str(ts) + ':' + str(i))

            time.sleep(1)

        except IndexError:
            break

        time.sleep(60)

def main():

    while True:

        dt = datetime.now()
        base_time = dt.replace(hour=0, minute=0, second=0)
        dayofweek = dt.weekday() + 1
        parent_dir = os.path.abspath(os.path.dirname(__file__))
        path = f'{parent_dir}/samples/daysofweek/{dayofweek}.csv'
        print(path)
        with open(path) as f:
            
            _ = next(f)
            payload = list(csv.reader(f,delimiter=';'))
            read_and_send( payload, base=base_time)

        if x == 6:
            x = 0
        else:
            x += 1

if __name__ == "__main__":

    main()