## load libraries
import os
import file_functions as ff

### Ensure that all csvs are closed before running the code or errors will be thrown

### This code appends to the current files (to allow for start where left off functionality), that means if you are rerunning from the beginning
###     you need to make sure to remove or rename old outputs so they don't get appended to

folder_for_runs = "highlycited"

os.chdir('../' + folder_for_runs + "/")

###########################################
### Extract generation 2 bibliographies ###
###########################################
# we may want to consider if there's a good way to remove duplicates here
# we already have a file linking the generation 2 entry to its bib right?

# prep to extract gen 2 bibs
ff.prep_for_python_tools(input_filepath = "citing-articles-loop/output_get_gen2.csv",
                      source_operation = "citing",
                      output_filepath = "cited-reference-loop/input_get_gen2_bibs.csv",
                      completed_filepath = "cited-reference-loop/get_gen2_bibs_completed_uts.csv")
#                         completed_filepath = "", # change if need to pick up where left off

# # not working for UT: WOS:000954750300001
                      

# extract gen 2 bibs
# feed this file into python cited script to get gen2 bibs

os.chdir('../' + folder_for_runs + '/cited-reference-loop')
os.system("python3 wosExpandedAPICitedReferences.py input_get_gen2_bibs.csv output_get_gen2_bibs.csv get_gen2_bibs_completed_uts.csv 2")

print("Extracted generation 2 bibliographies")
os.chdir('../') # two folders up from cited-reference-loop is highlycited which is where we want to be
##############################################
### Extract generation 2 bibliography info ###
##############################################

print("Prepping generation 2 bibs for info pull")
# prep gen2 bibs for exporter

ff.prep_for_automated_export(input_filepath = "cited-reference-loop/output_get_gen2_bibs.csv",
                          source_operation = "cited", 
                          output_filepath = "input_get_gen2_bibs_info.csv",
                          completed_filepath = "completed_uts_gen2_bibs_info.csv",
                          norecords_filepath = "norecords_uts_gen2_bibs_info.csv")

# now pull the information

ff.pull_full_records(input_filepath = "input_get_gen2_bibs_info.csv", 
                  output_filepath = "output_get_gen2_bibs_info.csv",
                 first_time = False, # if not True, be sure to have rerun the input file to shorten it
                 completed_filepath = "completed_uts_gen2_bibs_info.csv",
                 norecords_filepath = "norecords_uts_gen2_bibs_info.csv")
# got an error, not sure the cause, but restarted and seems to be fine?
print("Generation 2 bib info pulled")
