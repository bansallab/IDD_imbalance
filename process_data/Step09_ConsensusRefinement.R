# run this to establish consensus authorship and citation
{
  library(tidyverse)
  library(tidylog)
}

#### common info ####

# field definition
folder_path <- "highlycited"

acceptable_article_types <- c("Article", "Review", "Article; Early Access", 
                              "Review; Early Access", "Article; Proceedings Paper",
                              "Review; Book Chapter", "Article; Book Chapter",
                              "Article; Data Paper")

all_data <- read_csv(paste0(folder_path, "/df8_highlycited_0.7.csv")) %>% # was merged_cats for books
  dplyr::select(-`...1`) %>% 
  # mutate(FA_gender = ifelse(FA_gender == "male", "M",
  #                                ifelse(FA_gender == "female", "W", "U")),
  #        LA_gender = ifelse(LA_gender == "male", "M",
  #                                ifelse(LA_gender == "female", "W", "U"))) %>% 
  # mutate(AG_namsor = paste0(FA_gender, LA_gender)) %>% 
  relocate(c(FAFN, FALN, LAFN, LALN), .after = last_auth) %>% 
  relocate(c(AG, AG_namsor), .after = LALN) %>% 
  dplyr::select(-contains("Unnamed: 0"), -fa_fname, -la_fname) %>%  # comment for books
  rename(FA_prob_race2 = FA_prob2,
         LA_prob_race2 = LA_prob2) %>% 
  relocate(c(FA_race, FA_prob_race, FA_race2, FA_prob_race2,
             LA_race, LA_prob_race, LA_race2, LA_prob_race2), .after = LA_prob_gender) %>% 
  rename(FA_binrace = FA_group,
         LA_binrace = LA_group,
         ARbin_namsor = groups,
         FA_binrace_ethni = FA_group_e,
         LA_binrace_ethni = LA_group_e,
         ARbin_ethni = groups_e,
         FA_genderize = FA_group_g, 
         LA_genderize = LA_group_g) %>%  
  relocate(c(FA_genderize, LA_genderize), .after = AG) %>% 
  # these should have already been done so really no need to repeat but just in case
  filter(DT %in% acceptable_article_types, 
         PY < 2020)

# separate authorship and citation datasets
authorship <- read_csv(paste0(folder_path, "/authorship_dataset.csv")) %>% 
  dplyr::select(UT, PY) %>%  
  inner_join(all_data %>% dplyr::select(UT), by = "UT") %>% 
  ungroup() 

# 1 article not in authorship, citations showing up without authored paper, about SARS, not sure why dropped, group author
citation <- read_csv(paste0(folder_path, "/citation_dataset.csv")) %>% 
  dplyr::select(UT, in_bib_of_UT, PY) %>% 
  # filter to only include citations of valid citing articles, ie authorship still in df
  inner_join(all_data %>% rename(in_bib_of_UT = UT) %>% dplyr::select(in_bib_of_UT),
             by = "in_bib_of_UT") %>% 
  # filter to only include citations of valid article type, ie cited article is an article
  inner_join(all_data %>% dplyr::select(UT), by = "UT")

#### determine consensus authorship ####
# we need articles through 2023 for this to allow younger articles to be cited
# I went back and included those articles in these two datasets
# there is full article type filtering on these two, but can repeat in cas
authorship_2023 <- read_csv(paste0(folder_path, "/authorship_dataset_2023.csv")) %>% 
  filter(DT %in% acceptable_article_types) %>% 
  dplyr::select(UT, PY)
citation_2023 <- read_csv(paste0(folder_path, "/citation_dataset_2023.csv")) %>% 
  filter(DT %in% acceptable_article_types) %>% 
  dplyr::select(UT, in_bib_of_UT, PY)
# we might just need citation_2023 for this?
# authorship that appears in citation 
authorship_cited_once_23 <- citation_2023 %>% 
  inner_join(authorship %>% dplyr::select(UT), by = "UT") %>%
  dplyr::select(UT) %>% 
  distinct()

authorship_for_analysis <- authorship %>% inner_join(authorship_cited_once_23, by = "UT") %>% 
  filter(PY > 1999)

citation_for_analysis <- citation %>%
  inner_join(authorship_for_analysis %>% rename(in_bib_of_UT = UT) %>% 
               dplyr::select(in_bib_of_UT),
             by = "in_bib_of_UT")

percentile_test <- citation_for_analysis %>% filter(PY > 1999) %>%
  group_by(PY, UT) %>% summarise(num_citations = n())
percentiles <- percentile_test %>% group_by(PY) %>%
  summarise(q95 = quantile(num_citations, prob = 0.95),
            q90 = quantile(num_citations, prob = 0.90),
            q75 = quantile(num_citations, prob = 0.75),
            q50 = quantile(num_citations, prob = 0.50))
coef_95 <- lm(q95 ~ PY, data = percentiles)
coef_90 <- lm(q90 ~ PY, data = percentiles)
coef_75 <- lm(q75 ~ PY, data = percentiles)
coef_50 <- lm(q50 ~ PY, data = percentiles)

# percentile_test %>% ggplot(aes(x = PY, y = num_citations)) +
#   geom_jitter(alpha = 0.1) +
#   theme_bw() +
#   ylim(0, 20) + # removing some extremely highly cited
#   geom_point(data = percentiles, aes(y = q90), col = "darkgreen", shape = 24, size = 3, fill = "darkgreen") +
#   geom_abline(slope = coef_95$coefficients[[2]], intercept = coef_95$coefficients[[1]], col = "dodgerblue", linewidth = 1) +
#   geom_abline(slope = coef_75$coefficients[[2]], intercept = coef_75$coefficients[[1]], col = "pink", linewidth = 1) +
#   geom_abline(slope = coef_90$coefficients[[2]], intercept = coef_90$coefficients[[1]], col = "lightblue", linewidth = 1)

citation_func <- function(x, percentile = 0.95){
  if(percentile == 0.95){
    y <- coef_95$coefficients[[2]] * x + coef_95$coefficients[[1]]
  }else if(percentile == 0.90){
    y <- coef_90$coefficients[[2]] * x + coef_90$coefficients[[1]]
  }else if(percentile == 0.75){
    y <- coef_75$coefficients[[2]] * x + coef_75$coefficients[[1]]
  }else if(percentile == 0.50){
    y <- coef_50$coefficients[[2]] * x + coef_50$coefficients[[1]]
  }
  return(y)
}

times_cited <- citation_for_analysis %>% 
  group_by(UT, PY) %>% 
  summarise(TC_byfield = n()) %>% 
  ungroup() %>% 
  mutate(threshold_95 = round(citation_func(PY, 0.95)),
         threshold_90 = round(citation_func(PY, 0.9)),
         threshold_75 = round(citation_func(PY, 0.75)),
         threshold_50 = round(citation_func(PY, 0.5)),
         include_95 = TC_byfield >= threshold_95,
         include_90 = TC_byfield >= threshold_90,
         include_75 = TC_byfield >= threshold_75,
         include_50 = TC_byfield >= threshold_50) 

citation_output <- citation_for_analysis %>% 
  left_join(times_cited %>% select(UT, TC_byfield, include_95, include_90, include_75, include_50))

# filter based on desired threshold, for main text this is 90 percent
citation90 <- citation_for_analysis %>% 
  # filter all citations to only authorship + consensus cited citation papers (what we are deeming the field)
  inner_join(full_join(times_cited %>% 
                         filter(TC_byfield >= threshold_90) %>% 
                         mutate(well_cited = 1) %>% 
                         dplyr::select(UT, well_cited),
                       authorship_for_analysis %>% 
                         dplyr::select(UT) %>% 
                         mutate(author = 1))) 

citation75 <- citation_for_analysis %>% 
  # filter all citations to only authorship + consensus cited citation papers (what we are deeming the field)
  inner_join(full_join(times_cited %>% 
                         filter(TC_byfield >= threshold_75) %>% 
                         mutate(well_cited = 1) %>% 
                         dplyr::select(UT, well_cited),
                       authorship_for_analysis %>% 
                         dplyr::select(UT) %>% 
                         mutate(author = 1))) 


# four outputs
# citation has UT and in_bib_of_UT
# authorship
# all data
# citation just for alexes with 90th percentile and authorship
# 60thresh and 80thresh for sensitivity on gender/race cutoffs
# 75 is for 75th percentile for citation consensus
# ONLY ARTICLEDATA NEEDS TO BE SAVED DIFFERENTLY WITH THRESHOLDS
write_csv(all_data, paste0(folder_path, "/df9_articledata_0.7.csv"))
write_csv(authorship_for_analysis, paste0(folder_path, "/df9_consensus_authorship.csv"))
write_csv(citation_output, paste0(folder_path, "/df9_consensus_citation.csv"))
write_csv(citation90, paste0(folder_path, "/df9_relevant_citations_90.csv"))
write_csv(citation75, paste0(folder_path, "/df9_relevant_citations_75.csv"))


