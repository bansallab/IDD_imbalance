$ pip3 install -r requirements.txt

Python client to harvest Web of Science (WoS) citing articles.  The GET call requires a uniqueID parameter which must be a WoS UT (accession number/unique identifier).  This client downloads the full WoS bibliographic metadata records for all citing articles for a DOI.  Using this client does consume data that counts towards your yearly quota.  If you want to update times cited counts only, please use WoS Starter API (https://developer.clarivate.com/apis/wos-starter).

Usage:
$ python3 wosExpandedAPICiting.py INPUT_FILE_PATH

Note:  Sample CSV input file supplied (wosCitingInput.csv).  Input file must have a header row.

