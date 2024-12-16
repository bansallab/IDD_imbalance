library(tidyverse)
library(tidylog)
library(ggpubr)

### data functions
is_self_cite <- function(bib_first, bib_last, og_first, og_last){
  # bib_first and _last are first and last authors of cited paper
  # og_first and _last are first and last authors of citing paper
  if(is.na(bib_first) | is.na(bib_last) | is.na(og_first) | is.na(og_last)){
    return(NA)
  }else{
    if(tolower(og_first) %in% c(tolower(bib_first), tolower(bib_last)) | 
       tolower(og_last) %in% c(tolower(bib_first), tolower(bib_last))){
      return(1)
    }else{
      return(0)
    }
  }
}

who_self_cites <- function(bib_first, bib_last, og_first, og_last){
  # bib_first and _last are first and last authors of cited paper
  # og_first and _last are first and last authors of citing paper
  
  if(is.na(bib_first) | is.na(bib_last) | is.na(og_first) | is.na(og_last)){
    return(NA)
  }else if(tolower(og_first) %in% c(tolower(bib_first), tolower(bib_last))){
    return(1) # FA self cites
  }else if(tolower(og_last) %in% c(tolower(bib_first), tolower(bib_last))){
    return(2) # LA self cites
  }else{
    return(0)
  }
}


#### read in data ####
article_data <- read_csv("coreidd/df9_articledata_0.7.csv") %>% 
  mutate(global_north = ifelse(Country == "united kingdom (great britain)", 1, global_north))

authorship <- read_csv("coreidd/df9_consensus_authorship.csv") %>% 
  dplyr::select(UT) %>% 
  left_join(article_data, by = "UT") %>%
  ungroup() 

citation <- read_csv("coreidd/df9_consensus_citation.csv") %>% 
  dplyr::select(-PY) %>% 
  # filter to only include citations of valid citing articles
  inner_join(article_data %>% rename(in_bib_of_UT = UT) %>% dplyr::select(in_bib_of_UT),
             by = "in_bib_of_UT") %>%
  # join with cited article data
  inner_join(article_data, by = "UT") %>% 
  # link citation articles to their source bibliography information
  left_join(article_data %>% 
              rename(in_bib_of_AG = AG,
                     in_bib_of_ARbin_ethni = ARbin_ethni,
                     in_bib_of_UT = UT,
                     in_bib_of_first_auth = first_auth,
                     in_bib_of_last_auth = last_auth,
                     in_bib_of_country = Country,
                     in_bib_of_global_north = global_north,
                     in_bib_of_PY = PY) %>%
              dplyr::select(in_bib_of_UT, 
                            in_bib_of_AG, 
                            in_bib_of_ARbin_ethni,
                            in_bib_of_first_auth, 
                            in_bib_of_last_auth,
                            in_bib_of_country,
                            in_bib_of_global_north,
                            in_bib_of_PY)) %>% 
  rowwise() %>% 
  mutate(is_self_cite = is_self_cite(first_auth, 
                                     last_auth, 
                                     in_bib_of_first_auth, 
                                     in_bib_of_last_auth),
         who_self_cite = who_self_cites(first_auth, 
                                        last_auth, 
                                        in_bib_of_first_auth, 
                                        in_bib_of_last_auth)) 


#### make citation counts dfs ####

# times_cited_gn_nosf if filter is_self_cite == 0
# times_cited_gn_nosf_noauth if only include_90, no exceptions for authors
# times_cited_gn if no filter is_self_cite == 0
# times_cited_nosf if filter is_self_cite == 0 but no gn filter

# world but no self cite
times_cited_nosf <- citation %>% 
  left_join(authorship %>% dplyr::select(UT) %>% mutate(author = 1)) %>% 
  filter(include_90 | author == 1) %>%  # relevant citations
  filter(is_self_cite == 0) %>% # remove self citations
  group_by(UT, PY, TC_byfield, author) %>% 
  summarise(num_citations = n()) %>% # these values won't match TC_byfield bc excluding self cite
  ungroup() %>% 
  mutate(citation_rate = num_citations/(2024 - PY)) 

# no self cite but global north
times_cited_gn_nosf <- citation %>% 
  left_join(authorship %>% dplyr::select(UT) %>% mutate(author = 1)) %>% 
  filter(include_90 | author == 1) %>%  # relevant citations
  filter(is_self_cite == 0) %>% # remove self citations
  filter(global_north == 1) %>% #, in_bib_of_global_north == 1) %>% 
  group_by(UT, PY, TC_byfield, author) %>% 
  summarise(num_citations = n()) %>% # these values won't match TC_byfield bc excluding self cite
  ungroup() %>% 
  mutate(citation_rate = num_citations/(2024 - PY)) 

# no self cite but no auth exception  
times_cited_nosf_noauth <- citation %>% 
  left_join(authorship %>% dplyr::select(UT) %>% mutate(author = 1)) %>% 
  filter(include_90) %>%  # relevant citations
  filter(is_self_cite == 0) %>% # remove self citations
  group_by(UT, PY, TC_byfield, author) %>% 
  summarise(num_citations = n()) %>% # these values won't match TC_byfield bc excluding self cite
  ungroup() %>% 
  mutate(citation_rate = num_citations/(2024 - PY)) 

# world but with self cite
times_cited_yessf <- citation %>% 
  left_join(authorship %>% dplyr::select(UT) %>% mutate(author = 1)) %>% 
  filter(include_90 | author == 1) %>%  # relevant citations
  group_by(UT, PY, TC_byfield, author) %>% 
  summarise(num_citations = n()) %>% # these values won't match TC_byfield bc excluding self cite
  ungroup() %>% 
  mutate(citation_rate = num_citations/(2024 - PY)) 


#### get journal ISSNs from our data ####
# we need the issns to properly join with impact factor info
# library(readxl)
# exporter_data <- data.frame()
# for(i in 1:17){ # 27
#   print(i)
#   newd <- read_xlsx(path = paste0("../Alexes' Section/new_exporter/highly_cited/excel/export part", i, ".xlsx"), sheet = "ResearchOutput",
#                         col_types = c("text")) # reading all in as text cause easier to convert to # later
#   exporter_data <- exporter_data %>% bind_rows(newd)
# }
# 
# write_csv(exporter_data, "coreidd/new_exporter_data.csv")

exporter_data <- read_csv("coreidd/new_exporter_data.csv",
                              col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccc") %>%
  select(`Accession Number (UT)`, `Number of Cited References`, `Times Cited`, `Keywords`,
         `Keywords Plus`,
         `1st Subject Category (traditional)`, `2nd Subject Category (traditional)`,
         `Abstract`, `Published Year`, `ISSN`, `EISSN`, `Source title`,
         `Title`) %>%
  rename(UT = `Accession Number (UT)`)

refined_exporter_data <- article_data %>%
  left_join(exporter_data, by = "UT")

write_csv(refined_exporter_data, "coreidd/full_exporter_data_for_JIF.csv")

# which issns do we need to pull?
# needs to have dash for WoS/JCR
#### get exporter ISSNs to match to WoS ####
exporter_issns_for_jcr <- exporter_data %>% 
  # filter to our articles in 90th percentile + authorship
  left_join(citation %>% dplyr::select(UT, include_90)) %>% 
  left_join(authorship %>% dplyr::select(UT) %>% mutate(author = 1)) %>% 
  filter(include_90 | author == 1)  # relevant citations

  # # filter to unique journals
  # dplyr::select(ISSN, EISSN, `Source title`) %>% 
  # distinct() %>% 
  # # include both issn and eissn in case they don't have both
  # pivot_longer(cols = c(ISSN, EISSN), names_to = "issn_type", values_to = "issn") %>% 
  # filter(! is.na(issn)) %>% 
  # select(-issn_type) %>% 
  # distinct(issn) 

# I already downloaded some of these when testing out JCR, so don't redownload them
already_downloaded <- read_xlsx("highlycited/issns/batch0_240719.xlsx",
                                skip = 2) %>% 
  select(ISSN, eISSN) %>% 
  pivot_longer(cols = c(ISSN, eISSN), names_to = "issn_type", values_to = "issn") %>% 
  filter(issn != "N/A") %>% 
  distinct(issn)

issns_todo <- exporter_issns_for_jcr %>% 
  # want issns in exporter that aren't already downloaded
  anti_join(already_downloaded) %>% 
  mutate(batch_num = ceiling(row_number()/600))

write_csv(issns_todo, "highlycited/issns/exporter_issns_20240719.csv")
# input these issns into JCR and download their impact factors
# access JCR through medical school library databases
# go to journals, go to filter on LHS, and paste in ISSNs

#### read in JCRs!!! ####
# I manually deleted last two rows that have text & not data
library(readxl)
batchs <- seq(0, 5, by = 1)
jcr_df <- data.frame()
for(b in batchs){
  batch_data <- read_xlsx(paste0("highlycited/issns/batch", b, "_240719.xlsx"),
                          skip = 2) %>% 
    dplyr::select(ISSN, eISSN, `Journal name`, `2023 JIF`) %>% 
    rename(JIF = `2023 JIF`) %>% 
    distinct() %>% 
    mutate(ISSN = ifelse(ISSN == "N/A", NA, ISSN),
           eISSN = ifelse(eISSN == "N/A", NA, eISSN),
           JIF = ifelse(JIF == "N/A", NA, 
                        ifelse(JIF == "<0.1", 0.01,
                               JIF)),
           JIF = as.integer(JIF))
  
  jcr_df <- jcr_df %>% bind_rows(batch_data)
  
}

new_issns <- read_xlsx(paste0("highlycited/issns/batch1_241212.xlsx"),
                       skip = 2) %>%
  dplyr::select(ISSN, eISSN, `Journal name`, `2023 JIF`) %>%
  rename(JIF = `2023 JIF`) %>%
  distinct() %>%
  mutate(ISSN = ifelse(ISSN == "N/A", NA, ISSN),
         eISSN = ifelse(eISSN == "N/A", NA, eISSN),
         JIF = ifelse(JIF == "N/A", NA,
                      ifelse(JIF == "<0.1", 0.01,
                             JIF)),
         JIF = as.integer(JIF))

all_issns <- jcr_df %>%
  bind_rows(new_issns) %>%
  distinct()

#### join JIFs with data ####
# this is complicated because there's both eissns and issns and 
# some journals will mix them up, so join 4 times!
jcr_df_issns <- all_issns %>%
  mutate(ISSN = trimws(ISSN)) %>%
  select(-eISSN) %>%
  filter(! is.na(ISSN)) %>%
  distinct()

jcr_df_eissns <- all_issns %>%
  mutate(eISSN = trimws(eISSN)) %>%
  select(-ISSN) %>%
  filter(! is.na(eISSN)) %>%
  distinct()

issns_join <- exporter_issns_for_jcr %>%
  mutate(ISSN = trimws(ISSN),
         eISSN = trimws(EISSN)) %>%
  select(-EISSN) %>%
  left_join(jcr_df_issns %>%
              rename(journal_name_issn1 = `Journal name`,
                     JIF_issn1 = JIF), by = "ISSN") %>%
  left_join(jcr_df_eissns %>%
              rename(journal_name_eissn1 = `Journal name`,
                     JIF_eissn1 = JIF), by = "eISSN") %>%
  left_join(jcr_df_issns %>%
              rename(journal_name_issn2 = `Journal name`,
                     JIF_issn2 = JIF), by = c("eISSN" = "ISSN")) %>%
  left_join(jcr_df_eissns %>%
              rename(journal_name_eissn2 = `Journal name`,
                     JIF_eissn2 = JIF), by = c("ISSN" = "eISSN")) %>%
  mutate(JIF = ifelse(! is.na(JIF_issn1), JIF_issn1,
                      ifelse(! is.na(JIF_issn2), JIF_issn2,
                             ifelse(! is.na(JIF_eissn1), JIF_eissn1,
                                    JIF_eissn2)))) %>%
  select(UT, ISSN, eISSN, JIF, `Source title`)

write_csv(issns_join, "coreidd/uts_issns_241212.csv")

#### read in prepared data ####

uts_jifs <- read_csv("coreidd/uts_issns_241212.csv") %>% 
  select(-ISSN, -eISSN)
exporter_data <- read_csv("coreidd/full_exporter_data_for_JIF.csv")

#### create regression data ####

# note several different options for times_cited df in first line, run with each
# times_cited_nosf
# times_cited_gn_nosf
# times_cited_nosf_noauth
# times_cited_yessf

regression_df <- times_cited_yessf %>% 
  left_join(article_data, by = c("UT", "PY")) %>% 
  left_join(uts_jifs %>% select(UT, JIF)) %>% # was attempt_join
  left_join(exporter_data %>% select(UT, `Number of Cited References`, 
                                     `Times Cited`, `Keywords`, `Keywords Plus`, 
                                     `1st Subject Category (traditional)`,
                                     `2nd Subject Category (traditional)`, `Abstract`)) %>% 
  filter(! is.na(JIF)) %>% 
  filter(! grepl("U", AG)) %>%  # 36k papers, 24% under new constraints
  filter(! grepl("U", ARbin_ethni)) %>% # 55k papers, 65% under new constraints
  #filter(global_north == 1) %>% 
  rename(num_cited_refs = `Number of Cited References`) %>% 
  #filter(JIF > 0) %>% 
  mutate(AG = as.factor(AG),
         ARbin_ethni = as.factor(ARbin_ethni),
         log_num_citations = log(num_citations),
         log_citation_rate = log(citation_rate)) %>%
  dplyr::select(UT, SO, TI, Abstract, AG, ARbin_ethni, FA_genderize, LA_genderize,
                FA_binrace_ethni, LA_binrace_ethni,
                JIF, PY, num_citations,
                log_num_citations, citation_rate, log_citation_rate,
                WC, num_cited_refs, `1st Subject Category (traditional)`, Country,
                global_north) %>%
  filter(! is.na(global_north))
# mutate(across(c(num_citations, citation_rate, JIF, num_cited_refs, PY),
#               ~scale(.x)[,1], # scale adds [,1]to name
#               .names = "z_{.col}")) 
out <- regression_df %>% 
  rename(pair_gender = AG,
         pair_race = ARbin_ethni,
         FA_gender = FA_genderize,
         LA_gender = LA_genderize,
         FA_race = FA_binrace_ethni,
         LA_race = LA_binrace_ethni,
         year = PY,
         WC_1st = `1st Subject Category (traditional)`)
write_csv(out, "coreidd/input_jif_regression_yessf_20240925.csv")