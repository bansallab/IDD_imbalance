"""
Web of Science REST Expanded API citing articles client
"""

import requests
import json
import time

base_url = 'https://wos-api.clarivate.com/api/wos/citing'

def get_response(apikey, params, firstRecord=1, count=100, url=None):
    """
    Send the request to the WOS Expanded API for Citing Articles
    """
    time.sleep(1) # pause to avoid maxing out requests per unit time
    headers = {"Accept":"application/json", "X-ApiKey": apikey}
    print('Retrieving records {}'.format(count))
    #print(params)
    params['count'] = count
    params['firstRecord'] = firstRecord
    if url:
        req_url = base_url + url
    else:
        req_url = base_url
    print(req_url, params)
    r = requests.get(req_url, headers=headers, params=params)
    if r.status_code == 200:
        return r.json()
    elif r.status_code == 400:
        print("Bad request, omitting record", params['uniqueId'])
        return 400
    else:
        print('There was an ERROR')
        print(r.status_code) # 400 indicates bad request, 404 not found, 429 throttle error, 500 internal server error
        print(r.text)
        return None

def get_response_docs(apikey, params, firstRecord=1, count=100): # this is where we would need if/else
    """
    Retrieve all data for WoS UT
    """
    r = get_response(apikey, params, firstRecord, count)
    if r == 400: # error message will be thrown from get_response, but program will continue to run
        return [] # these UTs will be omitted from output, I've updated to omit only for bad request error code
    qr = r['QueryResult']
    if 'REC' in r["Data"]["Records"]["records"]: # if there's records, pull them
        data = r['Data']['Records']['records']['REC']
        #data = r['Data']
        if qr['RecordsFound'] > count:
            #qid = qr['QueryID']
            data = get_addl_results(apikey, params, qr['RecordsFound'],
                               (firstRecord + count), count, data)

        return data
    else: # if no citing articles or cited references, return empty dataset, we could change to say return empty dataset with key
        return []

def get_addl_results(apikey, params, recordsFound, firstRecord=1, count=100,
                     data=[]):
    headers = {"Accept":"application/json", "X-ApiKey": apikey}
    params['count'] = count

    while firstRecord <= recordsFound:
        params['firstRecord'] = firstRecord
        print(base_url, params)
        r = requests.get(base_url, headers=headers, params=params)
        if 'REC' in r.json()["Data"]["Records"]["records"]:
            if r.status_code == 200:
                data += r.json()["Data"]["Records"]["records"]["REC"]
                print('Retrieving {} of {}'.format(len(data), recordsFound))
                firstRecord += count
            else:
                print('Error! attempting to continue...')
                print(r.text)
                firstRecord += count

    return data


def get_all_records(apikey, params, firstRecord=1, count=100):
    """
    Retrieve all results for a query
    """
    r = get_response(apikey, params, firstRecord, count)
    qr = r['QueryResult']
    data = r['Data']['Records']['records']['REC']
    #data = r['Data']
    if qr['RecordsFound'] > count:
        #qid = qr['QueryID']
        data = get_addl_results(apikey, params, qr['RecordsFound'],
                               (firstRecord + count), count, data)

    return data
