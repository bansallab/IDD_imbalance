### Functions for citation project

### libraries ###
import os
import pandas as pd
import numpy as np
import csv
import requests
import time
# import urllib

# function to get UTs ready for the functions Clarivate provided us
def prep_for_python_tools(input_filepath, source_operation, output_filepath, completed_filepath = None):
    '''
    function to read in csv with UTs and output UTs in correct format for python code
    input_filepath leads to data file with UTs (must be csv)
    source_operation (how we got the input file) is either:
        "download" (from the online tool, already has UT column)
        "citing" (from the citing tool, column is `WoS Citing UT`)
    output_filepath example: "files/gen1_uts.csv" (must be csv)
    '''
    if source_operation == "download":
        # read in csv with column named UT
        df = pd.read_csv(input_filepath, usecols = ["UT"])
    elif source_operation == "citing":
        # read in csv with column named "WoS Citing UT" and rename it UT
        df = pd.read_csv(input_filepath, usecols = ["WoS Citing UT"]) 
        df = df.rename(columns = {"WoS Citing UT": "UT"}) # new name goes second
    # remove any rows with NA UTs
    df = df.dropna()
    
    # want to incorporate the start where left off part here
    # read in csv with completed UTs
    if completed_filepath is None:
        todo = df
    else:
        completed = pd.read_csv(completed_filepath, usecols = ["UT"])
        # do the antijoin for remaining UTs to do
        todo = df[~df.UT.isin(completed.UT)]        
    
    todo = todo.drop_duplicates()
    
    # write the dataframe to a csv with filepath output
    todo.to_csv(output_filepath, index = False)
    
    print("File is prepped for python")
    

def prep_for_automated_export(input_filepath, source_operation, output_filepath, completed_filepath = None, norecords_filepath = None):
    '''
    function to read in csv with UTs and output UTS in correct format for command line export
    input_filepath leads to data file with UTs (must be csv)
    source_operation (how we got the input file) is either:
        "download" (from the online tool, already has UT column)
        "citing" (from the citing tool, column is `WoS Citing UT`)
        "cited" (from the cited ref tool, column is `WoS Cited Reference UT`)
    output_filepath example: "files/gen1_uts.txt" (must be txt)
    '''
    
    # do the start where left off in this function instead of the big one   
    
    if source_operation == "download":
        # read in csv with column named UT
        df = pd.read_csv(input_filepath, usecols = ["UT"])
    elif source_operation == "citing":
        # read in csv with column named "WoS Citing UT" and rename it UT
        df = pd.read_csv(input_filepath, usecols = ["WoS Citing UT"]) 
        df = df.rename(columns = {"WoS Citing UT": "UT"}) # new name goes second
    elif source_operation == "cited":
        # read in csv with column named "WoS Cited Reference UT" and rename it UT
        # there will be non WoS articles in here but the exporter will remove them if you don't want to do so here
        df = pd.read_csv(input_filepath, usecols = ["WoS Cited Reference UT"]) 
        df = df.rename(columns = {"WoS Cited Reference UT": "UT"}) # new name goes second
        
    # check if already extracting so can pickup where left off
    if completed_filepath is None:
        todo = df
    else:
        completed = pd.read_csv(completed_filepath, usecols = ["UT"])
        norecords = pd.read_csv(norecords_filepath, usecols = ["UT"])
        # do the antijoin for remaining UTs to do
        together = pd.concat([completed, norecords])
        todo = df[~df.UT.isin(together.UT)]   
        
    df = todo.dropna().drop_duplicates()
    df = df[df['UT'].str.startswith("WOS:")]
    
    # we want only articles that are in WOS
    
    # write the dataframe to a csv with filepath output
    # can have header, no rownames, no quotes around these strings
    df.to_csv(output_filepath, index = None, sep = " ", mode = "w")
    
    print("File is prepped for export with " + str(len(df.index)) + " entries")

# the input file just has a column of UTs with the WOS prefix
# my own function to pull desired info from WoS API
def pull_full_records(input_filepath, output_filepath, first_time, completed_filepath, norecords_filepath):
    
    url = "https://wos-api.clarivate.com/api/wos/id"
    API_KEY = "INSERT YOUR CLARIVATE API KEY HERE" # need to add your own api key
    headers = {"X-ApiKey": API_KEY} # must pass API key in header?

    count = 1
    with open(output_filepath, mode="a") as f, open(completed_filepath, mode = 'a') as f2, open(norecords_filepath, mode = 'a') as f3: # was output.csv
        csv_writer = csv.writer(f)
        csv_writer2 = csv.writer(f2)
        csv_writer3 = csv.writer(f3)
        if first_time: # only write if starting new file
            csv_writer.writerow(["AF", "SO", "DI", "PY", "PD", 
                                 "DT", "WC", "UT", "TC", "PM"])
            # AF = authors, SO = journal, DI = doi, PY = pub year, DT = article type
            # TC = times cited, PM = pmid, PD = pub month
            # add column names first
            csv_writer2.writerow(["UT"])
            csv_writer3.writerow(["UT"])

        with open(input_filepath) as ut_file: # was python-test.csv
            # this would make a request for each row...ideally we'd do it all at once but not sure if it's possible
            print("Pulling information")
            for row in csv.DictReader(ut_file):
                #time.sleep(1)
                article_id = row["UT"]
                # article_query_string = urllib.parse.quote("UT={article_id}".format(article_id=article_id)) # not sure, but parse in case the "=" throws things off

                url_with_id = url + "/{article_id}".format(article_id=article_id) # pass article id(s) in the param part of URL, not the query
                # print(url_with_id)
                r = requests.get(url=url_with_id,
                                 headers=headers,
                                 params={"databaseId": "WOS",
                                         "count": "1", # required; must be 0-100
                                         "firstRecord": "1" # required; must be 1-100000
                                         })
                if r.text.split('\"RecordsFound\":', 1)[1][0] == "0":
                    #print("No records for ", article_id)
                    csv_writer3.writerow([article_id]) # write to no records filepath
                    continue # problem with this approach is this UT will continue to be queried since not counted as completed
                article_response = r.json() # returns response as a dict 
                #print(article_id)
                # print(article_response)

                # extract the info we need from JSON
                # some ref: http://help.incites.clarivate.com/wosWebServicesExpanded/appendix1Group/wosfieldNameTable.html

                #### AUTHORS ####
                # "name" = list of dicts, each corresponding to a type of name
                # specifically, those with author names have "role": "author"
                article_authors = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["names"]["name"]
                #num_authors = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["names"]["count"]

                if isinstance(article_authors, list): # if more than 1 author
                    # process author names: get full names; only works if >1 author
                    article_authors_proc = [author["full_name"] for author in article_authors if author["role"]=="author"]
                    article_authors_proc = "; ".join(article_authors_proc) # convert to a single string of author names separated by semicolon

                else:
                    if article_authors["role"] == "author":
                        article_authors_proc = article_authors['full_name']
                    else:
                        print("No individual authors for ", article_id) # group authors only, excluding but not printing
                        csv_writer2.writerow([article_id]) # adding this to completed one because wasn't a problematic pull, just problematic article type
                        print(count)
                        count += 1
                        continue # move on to next article

                # extract first and last authors
                # if num_authors > 1:
                    # last_author = article_authors[-1]['full_name']
                    # first_author = article_authors[0]['full_name']

                # else:
                    # first_author = article_authors['full_name']
                    # last_author = None
                    # article_authors_proc = first_author

                #### JOURNAL ####
                # "title" = list of dicts, each corresponding to a type of title
                # want the dict with "type": "source" since this holds the full Journal name
                article_journal_name = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["titles"]["title"] 
                # process titles: full journal name
                article_journal_name_proc = [journal_name["content"] for journal_name in article_journal_name if journal_name["type"]=="source"]
                article_journal_name_proc = "; ".join(article_journal_name_proc) # should just have one entry, but just in case

                #### DOI ####
                # "identifier" = list of dicts, each corresponding to a type of identifier
                # want dict with "type": "doi"
                try:
                    article_doi = article_response["Data"]["Records"]["records"]["REC"][0]["dynamic_data"]["cluster_related"]["identifiers"]["identifier"] 
                    article_pmid = None # maybe make this None?
                    # process identifiers: just DOI value
                    if isinstance(article_doi, list): 
                        article_doi_proc = [identifier["value"] for identifier in article_doi if identifier["type"]=="doi"]
                        if article_doi_proc == []: # if no doi, try x_ref doi
                            article_doi_proc = [identifier["value"] for identifier in article_doi if identifier["type"]=="xref_doi"]
                        # print(article_doi_proc)
                        if article_doi_proc == []: # if still empty, try pmid
                            article_pmid = [identifier["value"] for identifier in article_doi if identifier["type"]=="pmid"]
                            if article_pmid != []:
                                article_pmid = article_pmid[0].replace("MEDLINE:", "")
                            else:
                                article_pmid = None

                        article_doi_proc = "; ".join(article_doi_proc) # this way outputs nothing instead of empty list if no doi or pmid

                    else: # if not a list then only has one entry
                        if "doi" in article_doi.values():
                            article_doi_proc = article_doi['value']
                        elif "xref_doi" in article_doi.values():
                            article_doi_proc = article_doi['value']
                        elif "pmid" in article_doi.values():
                            article_pmid = article_doi['value']
                            article_pmid = article_pmid.replace("MEDLINE:", "")
                            article_doi_proc = None
                        else:
                            article_doi_proc = None

                    
                except TypeError: # if no identifiers
                    article_doi_proc = None    
                    article_pmid = None
                
                #### YEAR ####
                # this is already a single value
                article_year = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["pub_info"]["pubyear"]
                try:
                    article_month = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["pub_info"]["pubmonth"]
                except KeyError:
                    article_month = None

                #### ARTICLE TYPE ####
                num_article_types = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["doctypes"]["count"]
                if num_article_types > 1:
                    article_type = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["doctypes"]["doctype"]
                    article_type = "; ".join(article_type) # should just have one entry, but just in case
                else:
                    article_type = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["summary"]["doctypes"]["doctype"]

                #### CATEGORIES ####
                try:
                    article_categories = article_response["Data"]["Records"]["records"]["REC"][0]["static_data"]["fullrecord_metadata"]["category_info"]["subjects"]["subject"]
                    article_categories_proc = [subject["content"] for subject in article_categories if subject["ascatype"] == "traditional"]
                    article_categories_proc = "; ".join(article_categories_proc)
                except KeyError:
                    article_categories_proc = None
                
                #### TIMES CITED ####
                try:
                    article_times_cited = article_response["Data"]["Records"]["records"]["REC"][0]["dynamic_data"]["citation_related"]["tc_list"]["silo_tc"]["local_count"]
                except KeyError:
                    article_times_cited = None

                # now have the desired info; write to csv
                output_list = [article_authors_proc, article_journal_name_proc, 
                               article_doi_proc, article_year, article_month,
                               article_type, article_categories_proc, 
                               article_id, article_times_cited, article_pmid]
                csv_writer.writerow(output_list)
                csv_writer2.writerow([article_id])
                print(count)
                count += 1 # will not tally records which were unquery-able
            

        ut_file.close()
    f.close()
    f2.close()
    f3.close()

    print("Finished pulling article records")
