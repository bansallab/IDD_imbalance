source("HelperFunctions.R")
library(rjson);library(pbmcapply)

# choose based on field definition
folder_path <- "highlycited"
# Load in dataset from step 5
article.data <- readRDS(paste0(folder_path, "/df5_articledata_matchednames.rds"))

# Save number of cores on machine
cores=detectCores()

# If you have already started this process and saved an interim file, read it
# If not, create a new file to store names and gender probabilities
# Note: This process takes a while, so you might have to pause and pick
# it back up every once in a while
if("df6_namegends.rds"%in%list.files(paste0(folder_path, "/"))){
  namegends <- readRDS(file = paste0(folder_path, "/df6_namegends.rds"))
  print("fetched file")
}else{
  all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")
  first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                         authlist=all_auth_names,mc.cores=cores)
  
  # Isolate first- and last-authors' first names
  first_last_auths=pbmclapply(first_names,get.first.last,mc.cores=cores)
  
  # Get unique first names for gender estimation
  name_list=unique(unlist(first_last_auths))
  initials=unlist(lapply(name_list,is.initials))
  
  # Create dataset for names and predicted genders
  namegends=data.frame(name=name_list,
                       prob.m=rep(NA,length(name_list)),
                       prob.w=rep(NA,length(name_list)))
  
  # Make name variable chr type
  namegends$name=as.character(namegends$name)
  
  # Insert "-1" for initials, so you don't waste gender credits on them
  namegends$prob.m[initials==T]=-1
  namegends$prob.w[initials==T]=-1
  
  # Fill in some data using pre-built common names database
  commonnames=read.csv("CommonNamesDatabase.csv",stringsAsFactors=F)[,-1]
  names_in_common=which(namegends$name%in%commonnames$name)
  in_common_data=pbmclapply(names_in_common,match.common,
                            namegends,commonnames,mc.cores=cores)
  namegends[names_in_common,]=do.call(rbind,in_common_data)
  
  # Save this new dataset to be filled in as you go
  saveRDS(namegends,file=paste0(folder_path, "/df6_namegends.rds"))
  #save(namegends,file=paste0(folder_path, "/df6_namegends.RData"))
}

# Enter your API key here for genderize.io
# we will be using different API so need to modify the code accordingly
library(genderizeR)
# Run 'sum(is.na(namegends$prob.m))' to see how many credits you'll need
gender_api_key="YOUR API KEY"

# needed credits
sum(is.na(namegends$prob.m))
# Determine which names have yet to be queried from gender-api
r=which(is.na(namegends$prob.m))

#r = r[1:5]
count = 0
# For the remaining unqueried names...
for(i in r){
  
  # Isolate name to be queried
  this_name=namegends$name[i]
  
  json_data = genderizeAPI(this_name, apikey = gender_api_key)
  # Save gender probabilities to the namegends dataset
  # Enter -1 if no data for that name
  if(nrow(json_data$response) == 0){
    #is.na(json_data$response$gender)){ # need this here bc conditional can't be evaluated otherwise
    namegends$prob.w[i]=-1
    namegends$prob.m[i]=-1
  }else if(json_data$response$gender=="male"){
    namegends$prob.m[i]=json_data$response$probability #accuracy/100
    namegends$prob.w[i]=1-json_data$response$probability #accuracy/100
  }else if(json_data$response$gender=="female"){
    namegends$prob.w[i]=json_data$response$probability #accuracy/100
    namegends$prob.m[i]=1-json_data$response$probability #accuracy/100
  }
  
  # Output the name and associated gender probability (optional)
  print(count)
  count <- count + 1
  
  # Save the interim file so you can pick back up later if you decide to stop
  saveRDS(namegends, file=paste0(folder_path, "/df6_namegends.rds"))
  #save(namegends,file=paste0(folder_path, "/df6_namegends.RData"))
  
  # Pause to space out pull requests
  Sys.sleep(1)
  # time=round(runif(1,1,3),0)
  # for(t in time:1){
  #   Sys.sleep(1)
  #   #cat("Countdown:",t,"\n")
  # }
}

# I messed up somewhere and need to do this
library(tidyverse)
namegends <- namegends %>% distinct()
saveRDS(namegends, file=paste0(folder_path, "/df6_namegends.rds"))
