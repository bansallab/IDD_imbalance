{
  library(tidyverse)
  library(tidylog)
}

#### common info ####

folder_path <- "coreidd"

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

#### read in data #### 
article_data <- read_csv(paste0(folder_path, "/df9_articledata_0.7.csv")) %>% 
  mutate(article_id = row_number()) %>% 
  mutate(has_single_author = first_auth == last_auth) # this requires confidential info so must do here

mapping <- article_data %>% 
  select(UT, article_id)

authorship <- read_csv(paste0(folder_path, "/df9_consensus_authorship.csv")) %>% 
  left_join(article_data %>% select(UT, article_id), by = "UT") %>%
  ungroup() 

citation <- read_csv(paste0(folder_path, "/df9_consensus_citation.csv")) %>% 
  left_join(article_data %>% select(UT, article_id, first_auth, last_auth, has_single_author), by = "UT") %>% 
  left_join(article_data %>% select(UT, article_id, first_auth, last_auth) %>% 
              rename(in_bib_of_UT = UT, in_bib_of_article_id = article_id,
                     in_bib_of_first_auth = first_auth, in_bib_of_last_auth = last_auth),
            by = "in_bib_of_UT") %>% 
  rowwise() %>% 
  mutate(is_self_cite = is_self_cite(first_auth, 
                                     last_auth, 
                                     in_bib_of_first_auth, 
                                     in_bib_of_last_auth)) 

# to save
article_data_out <- article_data %>% select(article_id, PY, AG, ARbin_ethni, 
                                            FA_genderize, LA_genderize,
                                            FA_binrace_ethni, LA_binrace_ethni,
                                            Country, global_north, has_single_author)
authorship_out <- authorship %>% select(article_id, PY)
citation_out <- citation %>% select(article_id, in_bib_of_article_id, PY, include_95, include_90, include_75, is_self_cite)

write_csv(article_data_out, paste0(folder_path, "/df9_articledata_0.7_public.csv"))
write_csv(authorship_out, paste0(folder_path, "/df9_consensus_authorship_public.csv"))
write_csv(citation_out, paste0(folder_path, "/df9_consensus_citation_public.csv"))

write_csv(mapping, paste0(folder_path, "/key_ut_to_articleid.csv"))
