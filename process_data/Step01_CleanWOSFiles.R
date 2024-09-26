library(bibliometrix)
library(rvest)
library(tidyverse)
library(readr)
library(xml2) # read_xml didn't run without this
source("HelperFunctions.R")

# Select field definition -- Will need to go back and repeat for each definition!
folder_path <- "highlycited" # change to field definition

data.frame <- read_csv(paste0(folder_path, "/data_for_dworkin_pipeline.csv"),
                       col_types = "ccciccccic")
data.frame$done = 0

# # I think we can actually lump all of our data into one df
# # some DOIs need fixing, it's the xref DOI ones
# # mostly in gen1 bibs, only one in gen1

# Find article entries that don't have DOI but do have PubMed ID
# run this if starting over

without.DOI=which((data.frame$DI=="" | is.na(data.frame$DI)) & 
                    !is.na(data.frame$PM) & data.frame$done == 0)
count = 0
switch = 0
if(length(without.DOI)>0){
  
  # For articles with PubMed ID but no DOI
  for(j in without.DOI){
    
    # Find relevant DOI from PMC id-translator website
    this.pubmed=data.frame$PM[j]
    turl=paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
                this.pubmed)
    xml=suppressWarnings(read_xml(turl,as_html=T))
    doi=xml %>% html_nodes("record") %>% html_attr("doi")
    
    # If PMC id-translator doesn't have it indexed...
    if(is.na(doi)){
      # Try using pubmed website directly
      turl=paste0("https://pubmed.ncbi.nlm.nih.gov/",this.pubmed)
      
      # due to error here check if any have same doi and fix those
      try(
        {
          html=read_html(turl)
          doi=html %>% html_nodes("meta[name='citation_doi']") %>%
            html_attr("content")
          close_connection(turl)
        }
      )
    }
    
    # If neither thing worked, just make it empty
    doi=ifelse(!is.na(doi),doi,"")
    
    #print(showConnections(all = TRUE))
    # If it's not empty, enter the new DOI into data.frame
    if(nchar(doi)>0){
      data.frame$DI[j]=doi
      #print(doi)
      switch <- switch + 1
      if(switch %% 50 == 0){
        saveRDS(data.frame,file=paste0(folder_path, "/df1.rds"))
      }
      print(count)
    }
    #print(count)
    count <- count + 1
    # Pause to space out pull requests
    Sys.sleep(1)
    data.frame$done[j] = 1
  }
}

# Select relevant variables
# AF=authors, SO=journal, DT=article type, CR=reference list
# TC=total citation, PD=month/day, PY=year, DI=DOI
# missing CR, PD

# Translate month/day to numeric month
data.frame$PD=unlist(lapply(1:nrow(data.frame),get.date,pd=data.frame$PD))
data.frame$PD=as.numeric(data.frame$PD)
#data.frame=data.frame[data.frame$PD%in%c(1:12),]

# Standardize dois and reference lists to lowercase
data.frame$DI=tolower(data.frame$DI)
# data.frame$CR=tolower(data.frame$CR)

# Save new data frame of this journal's complete data
saveRDS(data.frame, file=paste0(folder_path, "/df1.rds"))






