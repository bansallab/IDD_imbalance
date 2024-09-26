"""
Script to produce CSV output from WoS Expanded API cited references endpoint using an input CSV file of WoS UTs.

Input CSV file example:

Accession Number (UT)
WOS:000902219500001
WOS:000890269100001
WOS:000888928500001
WOS:000878177600002
WOS:000869756200008

Example usage: python3 wosExpandedAPICitedReferences.py INPUT_FILE_PATH

"""
import csv
import sys
import argparse

import wosc.woscitedreferencesclient as wosc

# DEFAULT VALUES
#Hardcode API key here:
apikey = ' INSERT API KEY HERE '

params = {'databaseId': 'WOS',
            'uniqueId': 'WOS:000077253700008',
            'firstRecord': 1,
            'count': 100
            }

def parse_args(args):
    parser = argparse.ArgumentParser()
    parser.add_argument("INPUT_FILE", help="Input CSV file to parse.")
    parser.add_argument("OUTPUT_FILE", help="Output CSV file to parse.")
    parser.add_argument("COMPLETION_FILE", help="Completion CSV file to keep track of which UTs are done.")
    parser.add_argument("NEW_ATTEMPT", help="Is this your first attempt, so write header row? Or picking up where left off, so omit header?")
    parser.add_argument('-k', '--key', help="WoS Researcher API token.")

    return parser.parse_args(args)

def safeget(dct, *keys):
    for key in keys:
        try:
            dct = dct[key]
        except KeyError:
            return None
        except TypeError:
            pass
    if dct:
        try:
            if isinstance(dct, list):
                if isinstance(dct[0], dict):
                    try:
                        return dct[0][key]
                    except:
                        return dct[0]
                return dct[0]
            else:
                return dct
        except:
            return None

def parse_csv(file):
    f = open(file)
    csv_f = csv.reader(f, delimiter=',')

    #This skips the first row of the CSV file
    next(csv_f)

    data_set = []
    for row in csv_f:
        print('Row data: {}'.format(row))
        data_set.append((row[0]))

    return data_set

if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    # print(args)
    if args.key:
        apikey = args.key
    else:
        print("Using hardcoded API key")

    data_set = parse_csv(args.INPUT_FILE)
    #all_responses = {}

    with open(args.OUTPUT_FILE, 'a', newline = '') as f, open(args.COMPLETION_FILE, 'a', newline = '') as f2: # could change file output here
        csv_writer = csv.writer(f, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        header = ['WoS Source UT', 'WoS Cited Reference UT', 'Cited Author', 'Times Cited', 'Pub Year', 'Volume', 'Page', 'Cited Work', 'Cited Title', 'DOI']
        

        csv_writer2 = csv.writer(f2, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        header2 = ['UT']

        if args.NEW_ATTEMPT  == "1":
            csv_writer.writerow(header)
            csv_writer2.writerow(header2)

        for docs in data_set:
            params.update({'uniqueId': docs})
            rec = wosc.get_response_docs(apikey, params)
            #all_responses[docs] = rec
            
            for item in rec:
                row = []
                row.append(docs)
                row.append(safeget(item, 'UID'))
                row.append(safeget(item, 'CitedAuthor'))
                row.append(safeget(item, 'TimesCited'))
                row.append(safeget(item, 'Year'))
                row.append(safeget(item, 'Volume'))
                row.append(safeget(item, 'Page'))
                row.append(safeget(item, 'CitedWork'))
                row.append(safeget(item, 'CitedTitle'))
                row.append(safeget(item, 'DOI'))
                    
                csv_writer.writerow(row)
            
            row2 = []
            row2.append(docs)
            csv_writer2.writerow(row2)

    print("Done writing to CSV")


    

    # if len(all_responses) > 0:
    #     #print(rec)
    #     with open('wosCitedReferencesResults.csv', 'w', newline = '') as f: # could change file output here
    #         print("Mapping records to CSV.".format(len(rec)))
    #         csv_writer = csv.writer(f, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    #         header = ['WoS Source UT', 'WoS Cited Reference UT', 'Cited Author', 'Times Cited', 'Pub Year', 'Volume', 'Page', 'Cited Work', 'Cited Title', 'DOI']
    #         csv_writer.writerow(header)

    #         for key, rec in all_responses.items():
    #             for item in rec:
    #                 row = []
    #                 row.append(key)
    #                 row.append(safeget(item, 'UID'))
    #                 row.append(safeget(item, 'CitedAuthor'))
    #                 row.append(safeget(item, 'TimesCited'))
    #                 row.append(safeget(item, 'Year'))
    #                 row.append(safeget(item, 'Volume'))
    #                 row.append(safeget(item, 'Page'))
    #                 row.append(safeget(item, 'CitedWork'))
    #                 row.append(safeget(item, 'CitedTitle'))
    #                 row.append(safeget(item, 'DOI'))
                    
    #                 csv_writer.writerow(row)

    #     print("CSV written to wosCitedReferencesResults.csv")
    # else:
    #     print("*** No data to write :( ***".format(args.query))
