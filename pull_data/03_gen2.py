# load libraries
import os
import file_functions as ff

### Ensure that all csvs are closed before running the code or errors will be thrown

### This code appends to the current files (to allow for start where left off functionality),
###     that means if you are rerunning from the beginning
###     you need to make sure to remove or rename old outputs so they don't get appended to

folder_for_runs = "highlycited" # for field definition

os.chdir('../' + folder_for_runs + "/")

# ###################################################
# ### Extract generation 2 from generation 1 data ###
# ###################################################

# prep to extract gen 2 from gen 1
ff.prep_for_python_tools(input_filepath = "gen1_download.csv",
                      source_operation = "download",
                      output_filepath = "citing-articles-loop/input_get_gen2.csv")
#                       completed_filepath = "",

# extract gen 2,
# True means this is first attempt so start new completed UTs file
os.chdir('../' + folder_for_runs + '/citing-articles-loop')
os.system("python3 wosExpandedAPICiting.py input_get_gen2.csv output_get_gen2.csv get_gen2_completed_uts.csv 1")

print("Extracted generation 2 from generation 1 data")

#################################
### Extract generation 2 info ###
#################################
os.chdir('../' + folder_for_runs + "/")

print("Prepping generation 2 for info pull")
# prep gen2 for exporter

ff.prep_for_automated_export(input_filepath = "citing-articles-loop/output_get_gen2.csv",
                          source_operation = "citing", 
                          output_filepath = "input_get_gen2_info.csv",
                         completed_filepath = "completed_uts_gen2_info.csv",
                         norecords_filepath = "norecords_uts_gen2_info.csv")

# now pull the information

ff.pull_full_records(input_filepath = "input_get_gen2_info.csv", 
                  output_filepath = "output_get_gen2_info.csv",
                 first_time = False,
                 completed_filepath = "completed_uts_gen2_info.csv",
                 norecords_filepath = "norecords_uts_gen2_info.csv")
print("Generation 2 info pulled")
