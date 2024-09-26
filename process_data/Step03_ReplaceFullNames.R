source("HelperFunctions.R")
library(pbmcapply)
library(tidyverse)

# select field definition and run for each
folder_path <- "highlycited"

# Create empty data frame for all-journal data
article.data=NULL

# Load in original WoS data frame from step 1...
data.frame <- readRDS(paste0(folder_path, "/df1.rds")) %>% 
  mutate(DI = gsub("//", "/", DI))

# and new crossref names from step 2
new.names <- readRDS(paste0(folder_path, "/df2_missingnames.rds"))

# adding line to remove any missing author names (e.g. 10.1371/journal.pntd.0008771)
data.frame <- data.frame %>% filter(! is.na(AF))

# Separate out individual authors for each article
all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")

# Get first names of each author based on comma location
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)

# Find whether each first name only contains initials
initials=unlist(lapply(first_names,is.initials))

# Determine which articles only have initial information or an NA doi
bad_initial_rows <- which(initials != T) # we don't need to search these!
bad_doi_rows <- which(is.na(data.frame$DI)) # these DOIs don't work
all_bad_rows <- union(bad_initial_rows, bad_doi_rows) # no duplicates
valid_rows <- setdiff(seq(1:nrow(data.frame)), all_bad_rows)
needed_dois <- data.frame$DI[valid_rows]
needed_names <- all_auth_names[valid_rows]

# Replace entries with initials with the new names you got from crossref
data.frame$AF[valid_rows] = new.names$AF
#data.frame$AF[initials==T]=new.names$AF

# Append this new data to the full dataset
article.data=rbind(article.data,data.frame)

# # remove duplicates, not something Dworkin had to deal with
# library(tidyverse); library(tidylog)
# article.data <- article.data %>% distinct(UT, .keep_all = TRUE)

# Save out full dataset with info from all journals
saveRDS(article.data, file = paste0(folder_path, "/df3_articledata.rds"))

