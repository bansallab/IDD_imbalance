# load libraries
import os
import file_functions as ff

### First download generation 1 from the Web of Science online tool
### Download the cited reference output from Web of Science in Fast 5000 files and paste into one big spreadsheet, saved as `gen1_download.csv`

### Ensure that all csvs are closed before running the code or errors will be thrown

folder_for_runs = "coreidd" # for field definition

os.chdir('../' + folder_for_runs + "/")

#################################
### Extract generation 1 info ###
#################################

# Extract more detailed records 
print("Prepping generation 1 for info pull")
# prep gen1 for exporter
ff.prep_for_automated_export(input_filepath = "gen1_download.csv",
                          source_operation = "download", 
                          output_filepath = "input_get_gen1_info.csv",
                         completed_filepath = None)

# now pull the information

ff.pull_full_records(input_filepath = "input_get_gen1_info.csv", 
                  output_filepath = "output_get_gen1_info.csv",
                 first_time = True,
                 completed_filepath = "completed_uts_gen1_info.csv",
                 norecords_filepath = "norecords_uts_gen1_info.csv")
print("Generation 1 info pulled")
