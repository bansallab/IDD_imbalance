source("HelperFunctions.R")
library(rjson);library(pbmcapply)

# choose field definition
folder_path <- "highlycited"
# Load in article dataset from step 5
article.data <- readRDS(file=paste0(folder_path, "/df5_articledata_matchednames.rds"))

# Load in gender dataset from step 6
namegends <- readRDS(paste0(folder_path, "/df6_namegends.rds")) #_g1 for books I think

# Save number of cores on machine
cores=detectCores()

# Isolate first names of first- and last-authors
all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=cores)
first_last_auths=pbmclapply(first_names,get.first.last,mc.cores=cores)

# Assign probabilistic genders to author names
# Alexes is now (08/16/24) doing the thresholding, so just send her probabilities
library(tidyverse)
article.data$authors = first_last_auths
article.data <- article.data %>% 
  mutate(authors = gsub("c\\(", "", gsub("\\)", "", gsub("\"", "", authors)))) %>% 
  separate(authors, into = c("first_auth", "last_auth"), sep = ", ") %>% 
  left_join(namegends %>% rename(first_auth = name, prob.m.fa = prob.m, prob.w.fa = prob.w)) %>% 
  left_join(namegends %>% rename(last_auth = name, prob.m.la = prob.m, prob.w.la = prob.w)) %>% 
  rename(fa_fname = first_auth, la_fname = last_auth) %>% 
  rowwise() %>% 
  mutate(first_auth = head(strsplit(AF,"; ")[[1]], 1),
         last_auth = tail(strsplit(AF,"; ")[[1]], 1))


# 'Threshold' gives the probability above which you will assign a given gender
# This returns combinations of "M"=man, "W"=woman, and "U"=unknown
# giving e.g., "MW", "WM", "WU", "UU", etc. for each article
# article_auth_gends=pbmclapply(first_last_auths,gend.to.auths,
#                               namegends,threshold=0.7)

# Create new variable in article.data that gives author gender category
# article.data$AG=unlist(article_auth_gends)

# See proportion of articles for which gender could be assigned to both
# first and last author
# table(!grepl("U",article.data$AG))/nrow(article.data)

# Save new article data with gender categories
# saveRDS(article.data, file=paste0(folder_path, "/df7_articledata_withgenders.rds"))

write_csv(article.data, file=paste0(folder_path, "/df7_articledata_withgenders.csv"))