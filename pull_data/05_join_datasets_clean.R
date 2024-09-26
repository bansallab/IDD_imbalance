#### load libraries ####
{
  library(tidyverse)
  library(tidylog)
}

acceptable_article_types <- c("Article", "Review", "Article; Early Access", 
                              "Review; Early Access", "Article; Proceedings Paper",
                              "Review; Book Chapter", "Article; Book Chapter",
                              "Article; Data Paper")

#### books ####
folder_path <- "books"

# load data
# get gen 1
gen1 <- read_csv(paste0(folder_path, "/output_get_gen1_info.csv"),
                 col_types = "ccciccccic") %>% 
  mutate(gen = 1,
         PM = as.numeric(gsub("[][]", "", PM))) %>% 
  rowwise() %>% 
  mutate(DI = ifelse(grepl("10.7554|10.1371", DI), # if contains
                     strsplit(DI, "; ")[[1]][1], # split on ; 
                     DI),
         DI = gsub("; ", "", DI)) %>% 
  mutate(DI = ifelse(!is.na(DI) & !is.na(PM), NA, DI)) #%>% 
# should have done these here, but didn't necessarily
# especially because we wanted authorship through 2023 to determine consensus
  #filter(DT %in% acceptable_article_types) %>% 
  #filter(PY < 2020) # before 2020 

# get gen 1 bibliographies
gen1_bibs <- read_csv(paste0(folder_path, "/output_get_gen1_bibs_info.csv"),
                      col_types = "ccciccccic") %>% 
  mutate(gen = 1,
         PM = as.numeric(gsub("[][]", "", PM))) %>% 
  rowwise() %>% 
  mutate(DI = ifelse(grepl("10.7554|10.1371", DI), # if contains
                     strsplit(DI, "; ")[[1]][1], # split on ; 
                     DI),
         DI = gsub("; ", "", gsub("; ", "", DI))) %>%
  mutate(DI = ifelse(!is.na(DI) & !is.na(PM), NA, DI)) %>% 
  # should have done this here, but imposed more restrictions later instead
  #filter(DT %in% acceptable_article_types) %>% # must be article or review 
  filter(grepl("Article|Review", DT)) #%>% 
  # impose later instead
  #filter(PY < 2020) # before 2020 

### I HAD SOME BUGS TO DEAL WITH HERE
# 1.  xref dois didn't get joined properly, we need to remove "; " and replace with ""
# except eLife and Plos CB have subsections, they start with 10.7554 or 10.1371, 
# if contains that more than once, then cut after "; ", not all repeat
# 2. some articles with PMID were assigned DIs which makes no sense. we only get the PMID if no DOI,
# this only happens for the xref ones I think where it didn't realize it go

# get gen 1 bibs with the article that hosts them/cites them
gen1_bib_connect <- read_csv(paste0(folder_path, "/cited-reference-loop/output_get_gen1_bibs.csv"),
                             col_types = cols_only(
                               `WoS Source UT` = col_character(),
                               `WoS Cited Reference UT` = col_character()
                             )) %>% 
  rename(UT = `WoS Cited Reference UT`, in_bib_of_UT = `WoS Source UT`) %>% 
  filter(grepl("WOS:", UT)) # remove entries not in WoS
# link gen 1 bib information with their citing article
gen1_uts <- unique(gen1$UT)
gen1_bibs_connected <- gen1_bibs %>% 
  left_join(gen1_bib_connect) %>% 
  # this is slow
  filter(in_bib_of_UT %in% gen1_uts) # removed some non-articles from gen1, remove their refs

# save datasets
authorship_dataset <- gen1 %>% 
  distinct() %>% 
  mutate(PM = as.character(PM))
write_csv(authorship_dataset, paste0(folder_path, "/authorship_dataset.csv")) 

citation_dataset <- gen1_bibs_connected %>% 
  mutate(PM = as.character(PM))
write_csv(citation_dataset, paste0(folder_path, "/citation_dataset.csv")) 

all_data <- authorship_dataset %>% 
  bind_rows(citation_dataset %>% select(-in_bib_of_UT)) %>% 
  select(-gen) %>% 
  distinct(UT, .keep_all = TRUE)
write_csv(all_data, paste0(folder_path, "/data_for_dworkin_pipeline.csv")) 


#### highlycited ####
folder_path <- "highlycited" # use either highly cited or books

gen2 <- read_csv(paste0(folder_path, "/output_get_gen2_info.csv"),
                 col_types = "ccciccccic") %>%
  mutate(gen = 2,
         PM = as.numeric(gsub("[][]", "", PM))) %>%
  rowwise() %>%
  mutate(DI = ifelse(grepl("10.7554|10.1371", DI), # if contains
                     strsplit(DI, "; ")[[1]][1], # split on ;
                     DI),
         DI = gsub("; ", "", DI)) %>%
  mutate(DI = ifelse(!is.na(DI) & !is.na(PM), NA, DI)) %>%
  filter(DT %in% acceptable_article_types) %>% # must be article or review
  filter(PY < 2020) # before 2020, comment for authorship_2023

  
gen2_bibs <- read_csv(paste0(folder_path, "/output_get_gen2_bibs_info.csv"),
                      col_types = "ccciccccic") %>%
  mutate(gen = 2,
       PM = as.numeric(gsub("[][]", "", PM))) %>% # not sure what warning is about
  rowwise() %>%
  mutate(DI = ifelse(grepl("10.7554|10.1371", DI), # if contains
                   strsplit(DI, "; ")[[1]][1], # split on ;
                   DI),
       DI = gsub("; ", "", DI)) %>%
  mutate(DI = ifelse(!is.na(DI) & !is.na(PM), NA, DI)) %>%
  filter(DT %in% acceptable_article_types) %>%  # must be article or review
  filter(PY < 2020)
# 1 row offfrom 8/23/23 results likely because dropped 11,424 issue

# dois that differ are actually the same in tested sample
# get gen 2 bibs with the article that hosts them/cites them
gen2_bib_connect <- read_csv(paste0(folder_path, "/cited-reference-loop/output_get_gen2_bibs.csv"),
                             col_types = cols_only(
                               `WoS Source UT` = col_character(),
                               `WoS Cited Reference UT` = col_character()
                             )) %>%
  rename(UT = `WoS Cited Reference UT`, in_bib_of_UT = `WoS Source UT`) %>%
  filter(grepl("WOS:", UT)) # remove entries not in WoS

gen2_uts <- gen2 %>% select(UT)
gen2_bibs_connected_i <- gen2_bibs %>% left_join(gen2_bib_connect)
# gen2_bibs_connected <- gen2_bibs_connected_i %>%
#   filter(in_bib_of_UT %in% gen2_uts) # removed some non-articles from gen1, remove their refs
# could also try semi join of gen2_bibs_connected_i to gen2 %>% select(UT) where in_bib_of_UT = UT
gen2_bibs_connected <- gen2_bibs_connected_i %>%
  semi_join(gen2_uts, by = c("in_bib_of_UT" = "UT")) 
# I think this takes care of only citations of articles that were acceptable

authorship_dataset <- gen2 %>% 
  distinct() %>% 
  mutate(PM = as.character(PM))
write_csv(authorship_dataset, paste0(folder_path, "/authorship_dataset.csv")) 

citation_dataset <- gen2_bibs_connected %>% 
  mutate(PM = as.character(PM)) 
write_csv(citation_dataset, paste0(folder_path, "/citation_dataset.csv")) 

all_data <- authorship_dataset %>% 
  bind_rows(citation_dataset %>% select(-in_bib_of_UT)) %>% 
  select(-gen) %>% 
  distinct(UT, .keep_all = TRUE)

write_csv(all_data, paste0(folder_path, "/data_for_dworkin_pipeline.csv")) 


#### NOW GO TO SCRIPTS_STEPS AND START WITH STEP 1 ####