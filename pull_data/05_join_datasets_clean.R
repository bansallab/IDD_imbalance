#### load libraries ####
{
  library(tidyverse)
  library(tidylog)
}

acceptable_article_types <- c("Article", "Review", "Article; Early Access", 
                              "Review; Early Access", "Article; Proceedings Paper",
                              "Review; Book Chapter", "Article; Book Chapter",
                              "Article; Data Paper")

#### notes ####
# for books just need generation 1
# for highly cited just did generation 2
# for coreidd want generation 1 and 2


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


#### coreidd ####
folder_path <- "coreidd" 

# load data
# get titles
gen1_titles <- read_csv(paste0(folder_path, "/gen1_download.csv")) %>% 
  select(UT, TI) %>% 
  distinct()

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
  mutate(DI = ifelse(!is.na(DI) & !is.na(PM), NA, DI)) %>% 
  filter(DT %in% acceptable_article_types) %>% 
  left_join(gen1_titles, by = "UT")
# should have done these here, but didn't necessarily
# especially because we wanted authorship through 2023 to determine consensus
#filter(PY < 2020) # before 2020 



# get gen 1 bibliographies
# gen1_bib_titles <- read_csv(paste0(folder_path, "/cited-reference-loop/output_get_gen1_bibs.csv")) %>% 
#   rename(UT = `WoS Cited Reference UT`, TI = `Cited Title`) %>% 
#   select(UT, TI) %>% 
#   distinct()

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
  filter(DT %in% acceptable_article_types)
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
                               `WoS Cited Reference UT` = col_character(),
                               `Cited Title` = col_character()
                             )) %>% 
  rename(UT = `WoS Cited Reference UT`, in_bib_of_UT = `WoS Source UT`, TI = `Cited Title`) %>% 
  filter(grepl("WOS:", UT)) # remove entries not in WoS
# link gen 1 bib information with their citing article
gen1_uts <- unique(gen1$UT)
gen1_bibs_connected <- gen1_bibs %>% 
  left_join(gen1_bib_connect) %>% 
  # this is slow
  filter(in_bib_of_UT %in% gen1_uts) # removed some non-articles from gen1, remove their refs

# these are the ones we don't have titles for
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
  filter(DT %in% acceptable_article_types) # must be article or review
#filter(PY < 2020) # before 2020, comment for authorship_2023


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
  filter(DT %in% acceptable_article_types)  # must be article or review
#filter(PY < 2020)
# 1 row off from 8/23/23 results likely because dropped 11,424 issue

# dois that differ are actually the same in tested sample
# get gen 2 bibs with the article that hosts them/cites them
gen2_bib_connect <- read_csv(paste0(folder_path, "/cited-reference-loop/output_get_gen2_bibs.csv"),
                             col_types = cols_only(
                               `WoS Source UT` = col_character(),
                               `WoS Cited Reference UT` = col_character(),
                               `Cited Title` = col_character()
                             )) %>%
  rename(UT = `WoS Cited Reference UT`, in_bib_of_UT = `WoS Source UT`, TI = `Cited Title`) %>%
  filter(grepl("WOS:", UT)) # remove entries not in WoS

gen2_uts <- gen2 %>% select(UT)
gen2_bibs_connected_i <- gen2_bibs %>% left_join(gen2_bib_connect)
# gen2_bibs_connected <- gen2_bibs_connected_i %>%
#   filter(in_bib_of_UT %in% gen2_uts) # removed some non-articles from gen1, remove their refs
# could also try semi join of gen2_bibs_connected_i to gen2 %>% select(UT) where in_bib_of_UT = UT
gen2_bibs_connected <- gen2_bibs_connected_i %>%
  semi_join(gen2_uts, by = c("in_bib_of_UT" = "UT")) 
# I think this takes care of only citations of articles that were acceptable

# all titles
all_titles <- gen1 %>% select(UT, TI) %>% 
  bind_rows(gen1_bibs_connected %>% select(UT, TI)) %>% 
  bind_rows(gen2_bibs_connected %>% select(UT, TI)) %>% 
  distinct()

gen2_titles <- gen2 %>% 
  left_join(all_titles, by = "UT")
# 8,464 for which we still don't have titles. maybe wait to add until we see if these get dropped later
uts_missing_titles <- gen2_titles %>% 
  filter(is.na(TI)) %>% 
  select(UT) %>% 
  mutate(UT = gsub("WOS", "UT", UT))

write_delim(uts_missing_titles, paste0(folder_path, "/uts_for_exporter_titles.txt"))

# read in extra gen2 titles and bind them
library(readxl)
extra_titles <- read_xlsx(paste0(folder_path, "/missing-titles/30-11-2024_11-15/excel/export part1.xlsx"),
                          sheet = "ResearchOutput") %>% 
  rename(UT = `Accession Number (UT)`,
         TI = Title) %>% 
  select(UT, TI)

all_titles <- gen1 %>% select(UT, TI) %>% 
  bind_rows(gen1_bibs_connected %>% select(UT, TI)) %>% 
  bind_rows(gen2_bibs_connected %>% select(UT, TI)) %>%
  bind_rows(extra_titles) %>% 
  distinct()

gen2_titles <- gen2 %>% 
  left_join(all_titles, by = "UT")

gen2 <- gen2 %>% left_join(gen2_titles %>% select(UT, TI))

authorship_dataset <- gen1 %>% 
  bind_rows(gen2) %>% 
  select(-gen) %>% # should have done this but didn't
  distinct() %>% 
  mutate(PM = as.character(PM))
# this currently  (11/29/24) contains articles through 2024, (it says 2025)
write_csv(authorship_dataset, paste0(folder_path, "/authorship_dataset_full.csv")) 

citation_dataset <- gen1_bibs_connected %>% 
  bind_rows(gen2_bibs_connected) %>% 
  select(-gen) %>% # should have done this but didn't
  distinct(UT, in_bib_of_UT, .keep_all = TRUE) %>% # some duplicates w diff times cited
  mutate(PM = as.character(PM))
# this currently  (11/29/24) contains articles through 2024, (it says 2025)
write_csv(citation_dataset, paste0(folder_path, "/citation_dataset_full.csv")) 



# if we want to filter to before 2020, what do we need to do?
# we need to make sure the authorship article was published before 2020
# and that all citation articles were published before 2020
# and that all citation articles belong to citing articles published before 2020
authorship_dataset_pre2020 <- authorship_dataset %>% 
  filter(PY < 2020)
write_csv(authorship_dataset_pre2020, paste0(folder_path, "/authorship_dataset.csv")) 

citation_dataset_pre2020 <- citation_dataset %>% 
  filter(PY < 2020) %>% 
  semi_join(authorship_dataset_pre2020 %>% select(UT), by = c("in_bib_of_UT" = "UT"))
write_csv(citation_dataset_pre2020, paste0(folder_path, "/citation_dataset.csv")) 

all_data <- authorship_dataset_pre2020 %>% 
  bind_rows(citation_dataset_pre2020 %>% select(-in_bib_of_UT)) %>% 
  select(-gen) %>% 
  distinct(UT, .keep_all = TRUE)

write_csv(all_data, paste0(folder_path, "/data_for_dworkin_pipeline.csv")) 


#### NOW GO TO PROCESS_DATA AND START WITH STEP 1 ####

