{
  library(tidyverse)
  library(tidylog)
  library(pbmcapply)
  source("HelperFunctions.R")
  library(textclean)
  library(stringr)
  library(boot)
  library(ggpubr)
}

#### common info ####

cores=detectCores()

# change to desired field definition
folder_path <- "coreidd"

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


#### read in data #### 
article_data <- read_csv(paste0(folder_path, "/df9_articledata_0.7_public.csv")) %>% 
  mutate(global_north = ifelse(Country == "united kingdom (great britain)", 1, global_north)) %>% 
  filter(! has_single_author) # no single author articles

authorship <- read_csv(paste0(folder_path, "/df9_consensus_authorship_public.csv")) %>% 
  dplyr::select(article_id) %>% 
  inner_join(article_data, by = "article_id") %>% # non single authors
  ungroup() 

citation <- read_csv(paste0(folder_path, "/df9_consensus_citation_public.csv")) %>% 
  dplyr::select(-PY) %>% 
  # filter to only include citations of valid citing articles, this will be non-single author citers
  inner_join(article_data %>% rename(in_bib_of_article_id = article_id) %>% dplyr::select(in_bib_of_article_id),
             by = "in_bib_of_article_id") %>%
  # join with cited article data, non-single author citing
  inner_join(article_data, by = "article_id") %>% 
  # link citation articles to their source bibliography information
  left_join(article_data %>% 
              rename(in_bib_of_AG = AG,
                     in_bib_of_article_id = article_id,
                     # since using public data, next two lines are commented
                     # in_bib_of_first_auth = first_auth,
                     # in_bib_of_last_auth = last_auth,
                     in_bib_of_country = Country,
                     in_bib_of_global_north = global_north) %>%
              dplyr::select(in_bib_of_article_id, 
                            in_bib_of_AG, 
                            # since using public data, next two lines are commented
                            # in_bib_of_first_auth, 
                            # in_bib_of_last_auth,
                            in_bib_of_country,
                            in_bib_of_global_north)) %>% 
  # since using public data next two commands (rowwise & mutate) are commented as this has been performed
  # rowwise() %>% 
  # mutate(is_self_cite = is_self_cite(first_auth, 
  #                                    last_auth, 
  #                                    in_bib_of_first_auth, 
  #                                    in_bib_of_last_auth)) %>% 
  distinct() # none should be removed but just in case


#### stats for results ####
# authorship not excluding single authors
full_auth <- read_csv(paste0(folder_path, "/df9_consensus_authorship_public.csv")) %>% 
  select(article_id) %>% 
  inner_join(read_csv(paste0(folder_path, "/df9_articledata_0.7_public.csv")), by = "article_id")
# num authorship articles
nrow(full_auth)
citation90 <- read_csv(paste0(folder_path, "/df9_consensus_citation_public.csv")) %>% 
  select(article_id, in_bib_of_article_id, include_90) %>% 
  inner_join(read_csv(paste0(folder_path, "/df9_articledata_0.7_public.csv")), by = "article_id") %>% 
  left_join(full_auth %>% select(article_id) %>% mutate(author = 1)) %>% 
  filter(include_90 | author == 1)
# num distinct citation articles
citation90 %>% select(article_id) %>% distinct() %>% nrow()
# num total citations
citation90 %>% nrow()
# num countries
full_auth %>% group_by(Country) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n))
# frac written by GN senior authors
nrow(full_auth %>% filter(global_north == 1))/nrow(full_auth)
# senior author gender inferences
(full_auth %>% filter(! grepl("U", LA_genderize)) %>% nrow())/nrow(full_auth)
(citation90 %>% distinct(article_id, .keep_all = T) %>% filter(! grepl("U", LA_genderize)) %>% nrow())/nrow(citation90 %>% distinct(article_id))
# senior author race inferences
(full_auth %>% filter(! grepl("U", LA_ethni_race)) %>% nrow())/nrow(full_auth)
(citation90 %>% distinct(article_id, .keep_all = T) %>% filter(! grepl("U", LA_ethni_race)) %>% nrow())/nrow(citation90 %>% distinct(article_id))
# men lead authored
(full_auth %>% filter(grepl("M", FA_genderize)) %>% nrow())/nrow(full_auth)
# men senior authored
(full_auth %>% filter(grepl("M", LA_genderize)) %>% nrow())/nrow(full_auth)
# men lead and senior authored articles
(authorship %>% filter(grepl("M", FA_genderize) & grepl("M", LA_genderize)) %>% nrow())/nrow(authorship)
# women lead and senior authored articles
(authorship %>% filter(grepl("W", FA_genderize) & grepl("W", LA_genderize)) %>% nrow())/nrow(authorship)

#### intersectional analysis ####
library(MetBrewer)
la_plot_df <- authorship %>%
  dplyr::select(article_id, PY, LA_genderize, LA_binrace_ethni) %>% 
  filter(! grepl("U", LA_genderize)) %>% 
  filter(! grepl("U", LA_binrace_ethni)) %>% 
  mutate(LA_genderize = ifelse(LA_genderize == "M", "man", 
                               ifelse(LA_genderize == "W", "woman", "")),
         LA_binrace_ethni = ifelse(LA_binrace_ethni == "W", "white", 
                                   ifelse(LA_binrace_ethni == "N", "non-white", ""))) %>% 
  group_by(PY, LA_genderize, LA_binrace_ethni) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(PY, LA_genderize, LA_binrace_ethni, fill = list(count = 0)) %>%  
  group_by(PY) %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  mutate(intersectional_auth = paste(LA_binrace_ethni, LA_genderize)) %>% 
  mutate(prop = count/total) 
fa_plot_df <-  authorship %>%
  dplyr::select(article_id, PY, FA_genderize, FA_binrace_ethni) %>% 
  filter(! grepl("U", FA_genderize)) %>% 
  filter(! grepl("U", FA_binrace_ethni)) %>% 
  mutate(FA_genderize = ifelse(FA_genderize == "M", "man", 
                               ifelse(FA_genderize == "W", "woman", "")),
         FA_binrace_ethni = ifelse(FA_binrace_ethni == "W", "white", 
                                   ifelse(FA_binrace_ethni == "N", "non-white", ""))) %>% 
  group_by(PY, FA_genderize, FA_binrace_ethni) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(PY, FA_genderize, FA_binrace_ethni, fill = list(count = 0)) %>%  
  group_by(PY) %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  mutate(intersectional_auth = paste(FA_binrace_ethni, FA_genderize)) %>% 
  mutate(prop = count/total) 

la_plot_df$intersectional_auth <- factor(la_plot_df$intersectional_auth, 
                                         levels = c("white man", "non-white man",
                                                    "white woman", "non-white woman"))
fa_plot_df$intersectional_auth <- factor(fa_plot_df$intersectional_auth, 
                                         levels = c("white man", "non-white man",
                                                    "white woman", "non-white woman"))

la_plot_df %>% 
  ggplot(aes(x=PY, y=prop, fill=intersectional_auth)) + 
  geom_area(alpha=1,linewidth=.1,color=NA)+
  theme_bw() +
  labs(x = "Year", y = "Proportion of articles", fill = "Author identity") +
  scale_fill_manual(values = met.brewer("Archambault")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  ggtitle("Senior author") -> p2

fa_plot_df %>% 
  ggplot(aes(x=PY, y=prop, fill=intersectional_auth)) + 
  geom_area(alpha=1,linewidth=.1,color=NA)+
  theme_bw() +
  labs(x = "Year", y = "Proportion of articles", fill = "Author identity") +
  scale_fill_manual(values = met.brewer("Archambault")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  ggtitle("Lead author") -> p1

ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "right")
# ggsave("figures/for-pub/supp/intersectional-authorship.pdf", height = 4, width = 10)



#### revised functions ####
# some of these overwrite HelperFunctions.R

# this is the original function
transform.cat.binarygender=function(x){
  ifelse(x=="MM",0,
         ifelse(x=="WM",1,
                ifelse(x=="MW",2,
                       ifelse(x=="WW",3,NA))))
}
transform.cat.binaryrace=function(x){
  ifelse(x=="WW",0, # white white
         ifelse(x=="NW",1, # nonwhite white
                ifelse(x=="WN",2, # white nonwhite
                       ifelse(x=="NN",3,NA)))) # nonwhite nonwhite
}
transform.cat.binaryracepairs=function(x){
  ifelse(x=="WW",0, # white white
         ifelse(x %in% c("NW", "WN", "NN"), 1, # includes nonwhite author
                NA))
}

get.uncond.exp.general=function(x,group_cat,month_from_base, num_comb){
  # num_comb should = 4 for gender = 2 for binary race
  # x is a month
  if(!is.na(x)){
    if(x>min(month_from_base,na.rm=T)){ # if first month/paper then no expectation
      # basically genders of papers eligible to be cited at that time 
      #   (problematic with the way we have set up dataset of papers right now)
      citable_papers=group_cat[month_from_base<x]
      # number of papers in each authorship category
      prop_tab=table(factor(citable_papers,lev=0:(num_comb-1))) # this could be issue, seems like first value is 0
      # convert to proportions not counts, this is now expectation at that time
      expecs_uncond=prop_tab/sum(prop_tab)
      return(expecs_uncond)
    }else{
      return(rep(NA,num_comb))
    }
  }else{
    return(rep(NA,num_comb))
  }
}

# changing these functions because they refer to ref_tot_sub and I want that to be an argument
citeprops.general=function(ref_proportions, ref_tot_sub, group, num_comb, i=NULL,type='randomdraw'){
  # num_comb should = 4 for gender = 2 for binary race
  if(is.null(i)){i=1:nrow(ref_proportions)}
  # we aren't doing conditional so just taking that out for now
  if(type=='randomdraw'){
    # should sum across row, if != 0 has NAs, 
    #   this will be case for every row cause of columns 9-12 which are NA rn
    nas=apply(is.na(ref_proportions[i,c(1:(num_comb*2))]),1,sum)!=0 # change 8 to 12 when run GAM
    i=i[!nas]
    if(num_comb == 4){
      out=round(matrix(apply(ref_tot_sub[i,c(1:4,5:8)],2,sum)/ # sum down column with just eligible (non NA) rows
                         # total citations of each gender type, obs and expected
                         sum(apply(ref_tot_sub[i,1:4],2,sum)), # take ratio but sum all obs prop (nope now these are counts) cols? what?
                       # divide by total number of citations analyzed
                       byrow=T,ncol=4),3)
    }else if(num_comb == 2){
      out=round(matrix(apply(ref_tot_sub[i,c(1:4)],2,sum)/ # sum down column with just eligible (non NA) rows, changed since 4 cols
                         # total citations of each gender type, obs and expected
                         sum(apply(ref_tot_sub[i,1:2],2,sum)), # take ratio but sum all obs num papers 
                       # divide by total number of citations analyzed
                       byrow=T,ncol=2),10)
    }
    
    rownames(out)=c("Observed props.","Expected props.")
  }else{
    stop("'type' must be either 'conditional' or 'randomdraw'")
  }
  if(group == "gender"){
    colnames(out)=c("MM","WM","MW","WW")
  }else if(group == "race" & num_comb == 4){
    colnames(out)=c("WW","NW","WN","NN")
  }else if(group == "race" & num_comb == 2){
    colnames(out)=c("WW","PoC")
  }
  return(out)
}

citegap.general=function(ref_tot_sub, group, num_comb, i=NULL,type='randomdraw'){
  # num_comb should = 4 for gender = 2 for binary race
  # ratio of observed to expected prop of each gender paper type in ref list
  # really it's (obs - exp)/exp or obs/exp - 1
  if(is.null(i)){i=1:nrow(ref_tot_sub)}
  # removing conditional
  if(type=='randomdraw'){
    nas=apply(is.na(ref_tot_sub[i,c(1:(num_comb*2))]),1,sum)!=0 # change 8 to 12 when run GAM
    i=i[!nas] # remove rows with NA expectation
    if(num_comb == 4){
      out=100*(apply(ref_tot_sub[i,1:4],2,sum) / # col sums (that's why use total cites not props), take ratio - 1
                 # total obs citations of each gender type, below: divide by total expected citations of each gender type
                 apply(ref_tot_sub[i,5:8],2,sum) - 1)
    }else if(num_comb == 2){
      out=100*(apply(ref_tot_sub[i,1:2],2,sum) / # col sums (that's why use total cites not props), take ratio - 1
                 # total obs citations of each gender type, below: divide by total expected citations of each gender type
                 apply(ref_tot_sub[i,3:4],2,sum) - 1)
    }
    
  }else{
    stop("'type' must be either 'conditional' or 'randomdraw'")
  }
  if(group == "gender"){
    names(out)=c("MM","WM","MW","WW")
  }else if(group == "race" & num_comb == 4){
    names(out)=c("WW","NW","WN","NN")
  }else if(group == "race" & num_comb == 2){
    names(out)=c("WW","PoC")
  }
  return(out)
}

get.plotdf.general=function(boot, group, num_comb){
  # num_comb = 4 for gender, 2 for race
  if(group == "gender"){
    plotdf=data.frame(Group=c("Man,\nMan","Woman,\nMan",
                              "Man,\nWoman","Woman,\nWoman"),
                      Prop=boot$t0,
                      LB=apply(boot$t,2,quantile,.025),
                      UB=apply(boot$t,2,quantile,.975))
    plotdf$Group<-factor(plotdf$Group,
                         levels=c("Man,\nMan","Woman,\nMan",
                                  "Man,\nWoman","Woman,\nWoman"))
  }else if(group == "race" & num_comb == 4){
    plotdf=data.frame(Group=c("White,\nWhite","Non-White,\nWhite",
                              "White,\nNon-White","Non-White,\nNon-White"),
                      Prop=boot$t0,
                      LB=apply(boot$t,2,quantile,.025),
                      UB=apply(boot$t,2,quantile,.975))
    plotdf$Group<-factor(plotdf$Group,
                         levels=c("White,\nWhite","Non-White,\nWhite",
                                  "White,\nNon-White","Non-White,\nNon-White"))
  }else if(group == "race" & num_comb == 2){
    plotdf=data.frame(Group=c("White, White","PoC author"),
                      Prop=boot$t0,
                      LB=apply(boot$t,2,quantile,.025),
                      UB=apply(boot$t,2,quantile,.975))
    plotdf$Group<-factor(plotdf$Group,
                         levels=c("White, White","PoC author"))
  }
  
  return(plotdf)
}

#### function to prep over/under citation info for figure ####
generate_fig2 <- function(group_col, group, percentile_consensus, scope, cited_scope, num_comb){
  
  
  times_cited <- citation %>% 
    dplyr::select(article_id, include_95, include_90, include_75, include_50) %>% 
    distinct() %>% 
    dplyr::mutate(include = case_when(
      percentile_consensus == 0.95 ~ include_95,
      percentile_consensus == 0.9 ~ include_90,
      percentile_consensus == 0.75 ~ include_75,
      percentile_consensus == 0.5 ~ include_50,
      TRUE ~ TRUE
    )) %>% 
    dplyr::filter(include) %>% 
    mutate(well_cited = 1)

  # This is the other extreme where all citations are welcome, but they aren't included in the expectation
  expectation_data <- authorship %>%
    filter(case_when(cited_scope == "global" ~ TRUE,
                     cited_scope == "globalnorth" ~ global_north == 1,
                     cited_scope == "us" ~ Country == "usa",
                     cited_scope == "uk" ~ Country == "uk"))
  
  # fill in missing months
  expectation_data$PD[is.na(expectation_data$PD)] = 6 
  month_from_base=(expectation_data$PY-min(expectation_data$PY))+(expectation_data$PD/12) # years + months from first article
  unique_months=unique(month_from_base)
  
  # put categorizations into list
  if(group == "gender"){ # 2 gender categories
    # instead of coi could do [["string"]]
    # list of paper categories
    group_cat=unlist(pbmclapply(expectation_data[[group_col]],transform.cat.binarygender,mc.cores=cores)) # coi = column of interest
    
    # need this later but trying not to repeat conditionals
    obs_ref_props <- citation %>% 
      # filter all citations to only authorship + consensus cited citation papers (what we are deeming the field)
      inner_join(full_join(times_cited %>% dplyr::select(article_id, well_cited),
                           authorship %>% dplyr::select(article_id) %>% mutate(author = 1))) %>%
      rowwise() %>% 
      mutate(GC = transform.cat.binarygender({{group_col}})) %>% # group cat of citation
      filter(is_self_cite == 0, # make sure not self citation & genderized
             !is.na(GC)) %>% 
      # additional filter for scope: global, global north, us or uk -- except this needs to be on ppl doing citing
      filter(case_when(scope == "global" ~ TRUE,
                       scope == "globalnorth" ~ in_bib_of_global_north == 1,
                       scope == "us" ~ in_bib_of_country == "usa",
                       scope == "uk" ~ in_bib_of_country == "uk")) %>% 
      # add filter that can only cite papers of certain type
      filter(case_when(cited_scope == "global" ~ TRUE,
                       cited_scope == "globalnorth" ~ global_north == 1,
                       cited_scope == "us" ~ Country == "usa",
                       cited_scope == "uk" ~ Country == "uk")) %>%
      group_by(in_bib_of_article_id, GC) %>% 
      summarise(n = n()) %>% # count number of papers of each gender type cited by paper
      ungroup() %>% 
      complete(in_bib_of_article_id, GC, # var pairs to complete (in case no WW citations for ex, put 0)
               fill = list(n = 0)) %>% # replacement value 
      group_by(in_bib_of_article_id) %>% 
      mutate(tot_nonselfcite = sum(n)) %>% # total non self citations is sum of these citations
      ungroup() %>% 
      rowwise() %>% 
      mutate(obs_prop = n/tot_nonselfcite) %>% # prop citations of each gender type
      dplyr::select(-n) %>%
      pivot_wider(names_from = c("GC"), values_from = c("obs_prop"))
    
  }else if(group == "race" & num_comb == 4){
    # 2 race categories, 4 combinations
    group_cat=unlist(pbmclapply(expectation_data[[group_col]],transform.cat.binaryrace,mc.cores=cores))
    
    # need this later but trying not to repeat conditionals
    obs_ref_props <- citation %>% 
      inner_join(full_join(times_cited %>% dplyr::select(article_id, well_cited),
                           authorship %>% dplyr::select(article_id) %>% mutate(author = 1))) %>%
      rowwise() %>% 
      mutate(GC = transform.cat.binaryrace({{group_col}})) %>% # group cat
      filter(is_self_cite == 0, # make sure not self citation & genderized
             !is.na(GC)) %>% 
      # additional filter for scope: global, global north, us or uk -- except this needs to be on ppl doing citing
      filter(case_when(scope == "global" ~ TRUE,
                       scope == "globalnorth" ~ in_bib_of_global_north == 1,
                       scope == "us" ~ in_bib_of_country == "usa",
                       scope == "uk" ~ in_bib_of_country == "uk")) %>% 
      # add filter that can only cite papers of certain type
      filter(case_when(cited_scope == "global" ~ TRUE,
                       cited_scope == "globalnorth" ~ global_north == 1,
                       cited_scope == "us" ~ Country == "usa",
                       cited_scope == "uk" ~ Country == "uk")) %>%
      group_by(in_bib_of_article_id, GC) %>% 
      summarise(n = n()) %>% # count number of papers of each gender type cited by paper
      ungroup() %>% 
      complete(in_bib_of_article_id, GC, # var pairs to complete (in case no WW citations for ex, put 0)
               fill = list(n = 0)) %>% # replacement value 
      group_by(in_bib_of_article_id) %>% 
      mutate(tot_nonselfcite = sum(n)) %>% # total non self citations is sum of these citations
      ungroup() %>% 
      rowwise() %>% 
      mutate(obs_prop = n/tot_nonselfcite) %>% # prop citations of each gender type
      dplyr::select(-n) %>%
      pivot_wider(names_from = c("GC"), values_from = c("obs_prop"))
    
  }else if(group == "race" & num_comb == 2){
    # 2 race categories, 2 combinations
    group_cat=unlist(pbmclapply(expectation_data[[group_col]],transform.cat.binaryracepairs,mc.cores=cores))
    
    # need this later but trying not to repeat conditionals
    obs_ref_props <- citation %>% 
      inner_join(full_join(times_cited %>% dplyr::select(article_id, well_cited),
                           authorship %>% dplyr::select(article_id) %>% mutate(author = 1))) %>%
      rowwise() %>% 
      mutate(GC = transform.cat.binaryracepairs({{group_col}})) %>% # group cat
      filter(is_self_cite == 0, # make sure not self citation & genderized
             !is.na(GC)) %>% 
      # additional filter for scope: global, global north, us or uk -- except this needs to be on ppl doing citing
      filter(case_when(scope == "global" ~ TRUE,
                       scope == "globalnorth" ~ in_bib_of_global_north == 1,
                       scope == "us" ~ in_bib_of_country == "usa",
                       scope == "uk" ~ in_bib_of_country == "uk")) %>% 
      # add filter that can only cite papers of certain type
      filter(case_when(cited_scope == "global" ~ TRUE,
                       cited_scope == "globalnorth" ~ global_north == 1,
                       cited_scope == "us" ~ Country == "usa",
                       cited_scope == "uk" ~ Country == "uk")) %>%
      group_by(in_bib_of_article_id, GC) %>% 
      summarise(n = n()) %>% # count number of papers of each gender type cited by paper
      ungroup() %>% 
      complete(in_bib_of_article_id, GC, # var pairs to complete (in case no WW citations for ex, put 0)
               fill = list(n = 0)) %>% # replacement value 
      group_by(in_bib_of_article_id) %>% 
      mutate(tot_nonselfcite = sum(n)) %>% # total non self citations is sum of these citations
      ungroup() %>% 
      rowwise() %>% 
      mutate(obs_prop = n/tot_nonselfcite) %>% # prop citations of each gender type
      dplyr::select(-n) %>%
      pivot_wider(names_from = c("GC"), values_from = c("obs_prop"))
  }
  
  # Get unconditional gender proportions of citable papers (random draw model)
  # I.e., proportions for each gender category among all papers published before a given paper (by month)
  # for each month, apply get.uncond.exp function with arguments group_cat, month_from_base
  # generalizing this function so it accepts more than 4 levels
  uncond_expecs=pbmclapply(unique_months, get.uncond.exp.general, # apply get.uncond.exp.general func to unique_months list
                           group_cat, # this is the data, paper gender
                           month_from_base, 
                           num_comb,
                           mc.cores=cores)
  uncond_expecs=pbmclapply(month_from_base, match.uncond.exp,
                           uncond_expecs,unique_months,mc.cores=cores)
  uncond_expecs=do.call(rbind,uncond_expecs)
  uncond_expecs <- uncond_expecs %>% cbind(expectation_data %>% dplyr::select(article_id))
  
  # this is my own function to try to do what get.ref.props does with cited.papers
  # add filtering of authorship here to ppl whose practices we want to analyze (via inner_join)
  # not appropriate above bc expectation based on all
  if(num_comb == 4){
    ref_props <- authorship %>% 
      dplyr::select(article_id, PY, PD) %>% 
      # join with citation practices of that authorship paper 
      # [was left_join but some papers not in obs_ref_props if all citations have NA GC]
      inner_join(obs_ref_props, by = c("article_id" = "in_bib_of_article_id")) %>% 
      rename(obs_0 = `0`, obs_1 = `1`, obs_2 = `2`, obs_3 = `3`) %>%
      # join with expected citation practices given date of publication for authorship paper
      left_join(uncond_expecs) %>% 
      rename(uncond_expec_0 = `0`, uncond_expec_1 = `1`, uncond_expec_2 = `2`,
             uncond_expec_3 = `3`) %>%
      relocate(tot_nonselfcite, .after = uncond_expec_3) %>% 
      mutate(tot_nonselfcite = ifelse(is.na(tot_nonselfcite), 0, tot_nonselfcite))
    # uncond expec NAs are for the first papers in the dataset bc technically nothing was published then
  }else if(num_comb == 2){
    ref_props <- authorship %>% 
      dplyr::select(article_id, PY, PD) %>% 
      # join with citation practices of that authorship paper 
      # [was left_join but some papers not in obs_ref_props if all citations have NA GC]
      inner_join(obs_ref_props, by = c("article_id" = "in_bib_of_article_id")) %>% 
      rename(obs_0 = `0`, obs_1 = `1`) %>%
      # join with expected citation practices given date of publication for authorship paper
      left_join(uncond_expecs) %>% 
      rename(uncond_expec_0 = `0`, uncond_expec_1 = `1`) %>% 
      relocate(tot_nonselfcite, .after = uncond_expec_1) %>% # entirely removed cond_expecs from here
      mutate(tot_nonselfcite = ifelse(is.na(tot_nonselfcite), 0, tot_nonselfcite))
  }
  
  # reorganize proportions expected and observed
  ref_proportions <- data.matrix(ref_props %>% dplyr::select(-article_id, -PY, -PD))
  if(num_comb == 4){
    # mult by number of citations to get count of articles not props
    ref_tot_sub=ref_proportions[,1:8]*ref_proportions[,9] # changed to 8 and 9 from cond_expec removal
  }else if(num_comb == 2){
    ref_tot_sub=ref_proportions[,1:4]*ref_proportions[,5]
  }
  
  
  # Gap relative to overall literature, not sure why i had 1st arg as ref_tot_sub
  cite_props = citeprops.general(ref_proportions, ref_tot_sub, group, num_comb, type='randomdraw')
  cite_props %>% t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "RC") %>%
    pivot_longer(cols = c("Observed props.", "Expected props."),
                 names_to = "obs_or_expected", values_to = "proportion") %>%
    separate(obs_or_expected, into = c("prop_type", NA), sep = " ") %>%
    #filter(prop_type == "Observed") %>%
    ggplot(aes(x = RC, y = proportion, fill = prop_type)) +
    geom_col(alpha = 0.3, position = "dodge") -> my_plot
  
  print(cite_props) # this difference is not equal to the cite gap
  print(citegap.general(ref_tot_sub, group, num_comb, type='randomdraw')) # calc = (obs - exp)/exp
  # values are pretty close but not identical dep if use ref_proportions or ref_tot_sub, ref prop more extreme
  
  # Get bootstrap standard errors for gap values (applies citegap to bootstrap of ref_tot_sub values)
  # yielding ratio of observed to expected citation of each gender
  boot.rd = boot(ref_tot_sub, citegap.general, R=500, type='randomdraw', group = group, num_comb = num_comb)
  # we expect a certain number of MM citations to be given, but observe a different number
  # take the ratio of observed number of cites with expected number of cites --> percent over/under cite
  
  return(boot.rd)
  
}

#### make plots ####
gender_plot <- function(df, plot_limits = c(-32,12), plot_breaks = c(-30, -20, -10, 0, 10)){
  df %>% 
    ggplot(aes(x=Group, y=Prop, fill=Group))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    theme_bw()+
    theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 24, hjust = 0.5)) +
    xlab("Cited authors") +
    scale_fill_manual(values=c("Overall"="gray24",
                               "Man,\nMan"="#C0666B",
                               "Woman,\nMan"="#E3DFDA",
                               "Man,\nWoman"="#D4B383",
                               "Woman,\nWoman"="#47A8A5")) +
    ylab("Over- and undercitation (%)") +
    scale_y_continuous(breaks=plot_breaks,
                       limits=plot_limits,
                       expand = c(0,0)) -> p
  
  return(p)
  
}

race_plot <- function(df1, df2, plot_limits = c(-52,42), plot_breaks = c(-40, -20, 0, 20, 40)){
  df1 %>% 
    ggplot(aes(x=Group, y=Prop, fill=Group))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    geom_bar(data = df2, aes(color = Group), stat="identity", fill = NA, lty = "dashed") +
    geom_errorbar(data = df2, aes(ymin=LB,ymax=UB),width=.2) +
    theme_bw()+
    theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 24, hjust = 0.5)) +
    xlab("Cited authors") +
    scale_fill_manual(values=c("Overall"="gray24",
                               "PoC author"="#629550",
                               "White, White"="#5A5095")) +
    scale_color_manual(values=c("Overall"="gray24",
                                "PoC author"="#629550",
                                "White, White"="#5A5095")) +
    ylab("Over- and undercitation (%)") +
    scale_y_continuous(breaks=plot_breaks,
                       limits=plot_limits,
                       expand = c(0,0)) -> p
  
  return(p)
}


PERC_CONSENSUS <- 0.90

#### gender main ####
gender_main <- generate_fig2(expr(AG), "gender", percentile_consensus = PERC_CONSENSUS, 
                             scope = "global", cited_scope = "global", num_comb = 4) #+

gender_main_plot <- get.plotdf.general(gender_main, 'gender', num_comb = 4)

p <- gender_plot(gender_main_plot, plot_limits = c(-40, 12), plot_breaks = c(-40, -30, -20, -10, 0, 10))
p
# ggsave(paste0("figures/for-pub/", folder_path, "-fig2-gender.png"),
#        height = 4, width = 6, dpi = 600)

#### race main ####
race_global_cited <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                   scope = "globalnorth", cited_scope = "global", num_comb = 2) 
race_globalnorth_cited <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                        scope = "globalnorth", cited_scope = "globalnorth", num_comb = 2) 

race_global_cited_plot <- get.plotdf.general(race_global_cited, 'race', num_comb = 2)
race_globalnorth_cited_plot <- get.plotdf.general(race_globalnorth_cited, 'race', num_comb = 2)

p <- race_plot(race_globalnorth_cited_plot, race_global_cited_plot)
p
# ggsave(paste0("figures/for-pub/", folder_path, "-fig2-race.png"),
#        height = 4, width = 6)


#### consensus threshold ####

# gender
gender_supp_75 <- generate_fig2(expr(AG), "gender", percentile_consensus = 0.75, 
                                scope = "global", cited_scope = "global", num_comb = 4) #+

gender_supp_75_plot <- get.plotdf.general(gender_supp_75, 'gender', num_comb = 4)

p <- gender_plot(gender_supp_75_plot, plot_limits = c(-40, 12), plot_breaks = c(-40, -30, -20, -10, 0, 10))
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-gender-75perc.png"),
#        height = 4, width = 6, dpi = 600)

# race
race_global_cited_75_perc_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = 0.75, 
                                                scope = "globalnorth", cited_scope = "global", num_comb = 2) 
race_globalnorth_cited_75_perc_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = 0.75, 
                                                     scope = "globalnorth", cited_scope = "globalnorth", num_comb = 2) 

race_global_cited_75_perc_supp_plot <- get.plotdf.general(race_global_cited_75_perc_supp, 'race', num_comb = 2)
race_globalnorth_cited_75_perc_supp_plot <- get.plotdf.general(race_globalnorth_cited_75_perc_supp, 'race', num_comb = 2)

p <- race_plot(race_globalnorth_cited_75_perc_supp_plot, race_global_cited_75_perc_supp_plot)
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-race-75perc.png"),
#        height = 4, width = 6)


#### namsor ####
# gender
gender_supp_namsor <- generate_fig2(expr(AG_namsor), "gender", percentile_consensus = PERC_CONSENSUS, 
                                    scope = "global", cited_scope = "global", num_comb = 4) #+

gender_supp_namsor_plot <- get.plotdf.general(gender_supp_namsor, 'gender', num_comb = 4)

p <- gender_plot(gender_supp_namsor_plot, plot_limits = c(-55, 15), plot_breaks = c(-50, -40, -30, -20, -10, 0, 10)) 
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-gender-namsor.png"),
#        height = 4, width = 6, dpi = 600)

# race
race_global_cited_namsor_supp <- generate_fig2(expr(ARbin_namsor), "race", percentile_consensus = PERC_CONSENSUS, 
                                               scope = "globalnorth", cited_scope = "global", num_comb = 2) 
race_globalnorth_cited_namsor_supp <- generate_fig2(expr(ARbin_namsor), "race", percentile_consensus = PERC_CONSENSUS, 
                                                    scope = "globalnorth", cited_scope = "globalnorth", num_comb = 2) 

race_global_cited_namsor_supp_plot <- get.plotdf.general(race_global_cited_namsor_supp, 'race', num_comb = 2)
race_globalnorth_cited_namsor_supp_plot <- get.plotdf.general(race_globalnorth_cited_namsor_supp, 'race', num_comb = 2)

p <- race_plot(race_globalnorth_cited_namsor_supp_plot, race_global_cited_namsor_supp_plot, plot_limits = c(-32, 70),
               plot_breaks = c(-20, 0, 20, 40, 60))
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-race-namsor.png"),
#        height = 4, width = 6)

#### geography ####
# gender global north
gender_gn_supp <- generate_fig2(expr(AG), "gender", percentile_consensus = PERC_CONSENSUS, 
                                scope = "globalnorth", cited_scope = "global", num_comb = 4) #+

gender_gn_supp_plot <- get.plotdf.general(gender_gn_supp, 'gender', num_comb = 4)

p <- gender_plot(gender_gn_supp_plot, plot_limits = c(-40, 12), plot_breaks = c(-40, -30, -20, -10, 0, 10))
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-gender-gn-citers.png"),
#        height = 4, width = 6, dpi = 600)

# race, us only
globalnorth_cites_by_us <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                         scope = "us", cited_scope = "globalnorth", num_comb = 2) 

globalnorth_cites_by_us_plot <- get.plotdf.general(globalnorth_cites_by_us, 'race', num_comb = 2)

globalnorth_cites_by_us_plot %>% 
  ggplot(aes(x=Group, y=Prop, fill=Group))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
  theme_bw()+
  theme(legend.position="n")+
  geom_hline(yintercept=0,color='black',lty=1)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24, hjust = 0.5)) +
  xlab("Cited authors") +
  scale_fill_manual(values=c("Overall"="gray24",
                             "PoC author"="#629550",
                             "White, White"="#5A5095")) +
  ylab("Over- and undercitation (%)") +
  scale_y_continuous(breaks=c(-40, -20, 0, 20),
                     limits=c(-40,22),
                     expand = c(0,0)) #+ 
#scale_x_continuous(expand = c(0,0))
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-race-us.png"),
#        height = 4, width = 6)


####  vary identity threshold, RUN LAST ####
# we have not shared these data files publicly hence the use of UT
# 0.8
article_data <- read_csv(paste0(folder_path, "/df9_articledata_0.8.csv")) %>% 
  mutate(global_north = ifelse(Country == "united kingdom (great britain)", 1, global_north)) %>% 
  #mutate(Country = ifelse(Country %in% c("england", "scotland", "wales"), "uk", Country)) %>% 
  filter(first_auth != last_auth) %>% # no single author articles
  filter(!is.na(Country))

authorship <- read_csv(paste0(folder_path, "/df9_consensus_authorship.csv")) %>% 
  dplyr::select(UT) %>% 
  inner_join(article_data, by = "UT") %>% # non single authors
  ungroup() 

citation <- read_csv(paste0(folder_path, "/df9_consensus_citation.csv")) %>% 
  dplyr::select(-PY) %>% 
  # filter to only include citations of valid citing articles, this will be non-single author citers
  inner_join(article_data %>% rename(in_bib_of_UT = UT) %>% dplyr::select(in_bib_of_UT),
             by = "in_bib_of_UT") %>%
  # join with cited article data, non-single author citing
  inner_join(article_data, by = "UT") %>% 
  # link citation articles to their source bibliography information
  left_join(article_data %>% 
              rename(in_bib_of_AG = AG,
                     in_bib_of_UT = UT,
                     in_bib_of_first_auth = first_auth,
                     in_bib_of_last_auth = last_auth,
                     in_bib_of_country = Country,
                     in_bib_of_global_north = global_north) %>%
              dplyr::select(in_bib_of_UT, 
                            in_bib_of_AG, 
                            in_bib_of_first_auth, 
                            in_bib_of_last_auth,
                            in_bib_of_country,
                            in_bib_of_global_north)) %>% 
  rowwise() %>% 
  mutate(is_self_cite = is_self_cite(first_auth, 
                                     last_auth, 
                                     in_bib_of_first_auth, 
                                     in_bib_of_last_auth)) %>% 
  distinct() # DIFF

gender_80_thresh_supp <- generate_fig2(expr(AG), "gender", percentile_consensus = PERC_CONSENSUS, 
                                       scope = "global", cited_scope = "global", num_comb = 4) #+

gender_80_thresh_supp_plot <- get.plotdf.general(gender_80_thresh_supp, 'gender', num_comb = 4)

p <- gender_plot(gender_80_thresh_supp_plot, plot_limits = c(-40, 12),
                 plot_breaks = c(-40, -30, -20, -10, 0, 10))
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-gender-0.8.png"),
#        height = 4, width = 6, dpi = 600)

# race
race_global_cited_80_thresh_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                                  scope = "globalnorth", cited_scope = "global", num_comb = 2) 
race_globalnorth_cited_80_thresh_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                                       scope = "globalnorth", cited_scope = "globalnorth", num_comb = 2) 

race_global_cited_80_thresh_supp_plot <- get.plotdf.general(race_global_cited_80_thresh_supp, 'race', num_comb = 2)
race_globalnorth_cited_80_thresh_supp_plot <- get.plotdf.general(race_globalnorth_cited_80_thresh_supp, 'race', num_comb = 2)

p <- race_plot(race_globalnorth_cited_80_thresh_supp_plot, race_global_cited_80_thresh_supp_plot)
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-race-0.8.png"),
#        height = 4, width = 6)


# 0.6

article_data <- read_csv(paste0(folder_path, "/df9_articledata_0.6.csv")) %>% 
  mutate(global_north = ifelse(Country == "united kingdom (great britain)", 1, global_north)) %>% 
  filter(first_auth != last_auth) %>% # no single author articles
  filter(! is.na(Country))

authorship <- read_csv(paste0(folder_path, "/df9_consensus_authorship.csv")) %>% 
  dplyr::select(UT) %>% 
  inner_join(article_data, by = "UT") %>% # non single authors
  ungroup() 

citation <- read_csv(paste0(folder_path, "/df9_consensus_citation.csv")) %>% 
  dplyr::select(-PY) %>% 
  # filter to only include citations of valid citing articles, this will be non-single author citers
  inner_join(article_data %>% rename(in_bib_of_UT = UT) %>% dplyr::select(in_bib_of_UT),
             by = "in_bib_of_UT") %>%
  # join with cited article data, non-single author citing
  inner_join(article_data, by = "UT") %>% 
  # link citation articles to their source bibliography information
  left_join(article_data %>% 
              rename(in_bib_of_AG = AG,
                     in_bib_of_UT = UT,
                     in_bib_of_first_auth = first_auth,
                     in_bib_of_last_auth = last_auth,
                     in_bib_of_country = Country,
                     in_bib_of_global_north = global_north) %>%
              dplyr::select(in_bib_of_UT, 
                            in_bib_of_AG, 
                            in_bib_of_first_auth, 
                            in_bib_of_last_auth,
                            in_bib_of_country,
                            in_bib_of_global_north)) %>% 
  rowwise() %>% 
  mutate(is_self_cite = is_self_cite(first_auth, 
                                     last_auth, 
                                     in_bib_of_first_auth, 
                                     in_bib_of_last_auth)) %>% 
  distinct() # DIFF

gender_60_thresh_supp <- generate_fig2(expr(AG), "gender", percentile_consensus = PERC_CONSENSUS, 
                                       scope = "global", cited_scope = "global", num_comb = 4) #+

gender_60_thresh_supp_plot <- get.plotdf.general(gender_60_thresh_supp, 'gender', num_comb = 4)

p <- gender_plot(gender_60_thresh_supp_plot, plot_limits = c(-40, 12),
                 plot_breaks = c(-40, -30, -20, -10, 0, 10))
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-gender-0.6.png"),
#        height = 4, width = 6, dpi = 600)

race_global_cited_60_thresh_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                                  scope = "globalnorth", cited_scope = "global", num_comb = 2) 
race_globalnorth_cited_60_thresh_supp <- generate_fig2(expr(ARbin_ethni), "race", percentile_consensus = PERC_CONSENSUS, 
                                                       scope = "globalnorth", cited_scope = "globalnorth", num_comb = 2) 

race_global_cited_60_thresh_supp_plot <- get.plotdf.general(race_global_cited_60_thresh_supp, 'race', num_comb = 2)
race_globalnorth_cited_60_thresh_supp_plot <- get.plotdf.general(race_globalnorth_cited_60_thresh_supp, 'race', num_comb = 2)

p <- race_plot(race_globalnorth_cited_60_thresh_supp_plot, race_global_cited_60_thresh_supp_plot)
p
# ggsave(paste0("figures/for-pub/supp/", folder_path, "-fig2-race-0.6.png"),
#        height = 4, width = 6)
