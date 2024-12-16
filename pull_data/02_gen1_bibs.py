# load libraries
import os
import file_functions as ff

### Ensure that all csvs are closed before running the code or errors will be thrown

### This code appends to the current files (to allow for start where left off functionality),
###     that means if you are rerunning from the beginning you need to make sure to remove or 
###     rename old outputs so they don't get appended to

folder_for_runs = "coreidd" # for field definition

os.chdir('../' + folder_for_runs + "/")

###########################################
### Extract generation 1 bibliographies ###
###########################################

# gen1_download will always have everything
# if there's a completed-uts file, read that in as well, only remaining UTs will be outputted
# it will be on you to keep the completed file updated, we may want to change its name

# prep to extract gen 1 bibs
ff.prep_for_python_tools(input_filepath = "gen1_download.csv",
                      source_operation = "download",
                      output_filepath = "cited-reference-loop/input_get_gen1_bibs.csv")
                      #completed_filepath = "cited-reference-loop/get_gen1_bibs_completed_uts.csv")

# extract gen 1 bibs
# input: input_get_gen1_bibs.csv
# output: output_get_gen1_bibs.csv
# APICited References takes in input file, output file, completed-UTs file, and number
#    for whether this is first attempt (i.e. write header or not), 1 means write header, anything else don't
os.chdir('../' + folder_for_runs + '/cited-reference-loop')
os.system("python3 wosExpandedAPICitedReferences.py input_get_gen1_bibs.csv output_get_gen1_bibs.csv get_gen1_bibs_completed_uts.csv 1")

# it printed done in the Jupyter Notebook terminal, but only 0 here
# I interrupted and it printed 2, let's see if the completed ones are saving - yes that works!
print("Extracted generation 1 bibliographies")

### FYI if you use open(, "x") that only writes if the file doesn't exist

##############################################
### Extract generation 1 bibliography info ###
##############################################
os.chdir('../')
#os.chdir('../' + folder_for_runs + "/") # this will cause an error if above chdir is uncommented, need to get back to highly cited
# shortcut is if error is thrown and file stops, recomment above and rerun file, need a better solution for this obviously

print("Prepping generation 1 bibs for info pull")
# prep gen1 bibs for exporter

ff.prep_for_automated_export(input_filepath = "cited-reference-loop/output_get_gen1_bibs.csv",
                          source_operation = "cited", 
                          output_filepath = "input_get_gen1_bibs_info.csv",
                          completed_filepath = None, #"completed_uts_gen1_bibs_info.csv",
                          norecords_filepath = None) #"norecords_uts_gen1_bibs_info.csv")

# now pull the information

ff.pull_full_records(input_filepath = "input_get_gen1_bibs_info.csv", 
                  output_filepath = "output_get_gen1_bibs_info.csv",
                  first_time = True,
                  completed_filepath = "completed_uts_gen1_bibs_info.csv",
                 norecords_filepath = "norecords_uts_gen1_bibs_info.csv")
print("Generation 1 bibs info pulled")
