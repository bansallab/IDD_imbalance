"""
Web of Science REST Expanded API cited references client
"""

import requests
import json
import time

base_url = 'https://wos-api.clarivate.com/api/wos/references'

def get_response(apikey, params, firstRecord=1, count=100, url=None):
    """
    Send the request to the WOS Expanded API for Cited References
    """
    time.sleep(2)
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
    else:
        print('There was an ERROR')
        print(r.status_code)
        print(r.text)
        return None

def get_response_docs(apikey, params, firstRecord=1, count=100):
    """
    Retrieve all data for WoS UT
    """
    r = get_response(apikey, params, firstRecord, count)
    qr = r['QueryResult']
    data = r['Data']
    if qr['RecordsFound'] > count:
        #qid = qr['QueryID']
        data = get_addl_results(apikey, params, qr['RecordsFound'],
                               (firstRecord + count), count, data)

    return data


def get_addl_results(apikey, params, recordsFound, firstRecord=1, count=100,
                     data=[]):
    headers = {"Accept":"application/json", "X-ApiKey": apikey}
    params['count'] = count

    while firstRecord <= recordsFound:
        params['firstRecord'] = firstRecord
        print(base_url, params)
        r = requests.get(base_url, headers=headers, params=params)
        if r.status_code == 200:
            data += r.json()["Data"]
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
    data = r['Data']
    if qr['RecordsFound'] > count:
        #qid = qr['QueryID']
        data = get_addl_results(apikey, params, qr['RecordsFound'],
                               (firstRecord + count), count, data)

    return data
