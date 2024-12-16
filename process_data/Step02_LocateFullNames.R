source("HelperFunctions.R")

{
  library(pbmcapply)
  library(rvest)
  library(RJSONIO);
  library(textclean)
  library(tidyverse)
  library(magrittr)
}


# Select definition -- Will need to go back and repeat for each definition!
folder_path <- "coreidd"

data.frame <- readRDS(paste0(folder_path, "/df1.rds")) %>% 
  mutate(DI = gsub("//", "/", DI)) %>% 
  filter(! is.na(AF))
# adding line to remove any missing author names 

# Separate out individual authors for each article
all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")

# Get first names of each author based on comma location
# run & save so don't have to repeat
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)
saveRDS(first_names, paste0(folder_path, "/step2_firstnames.rds"))
# first_names <- readRDS(paste0(folder_path, "/step2_firstnames.rds"))

# Find whether each first name only contains initials
initials=unlist(lapply(first_names,is.initials))

# Determine which articles only have initial information or an NA doi
bad_initial_rows <- which(initials != T) # we don't need to search these, we have the full name!
bad_doi_rows <- which(is.na(data.frame$DI)) # these DOIs don't work
all_bad_rows <- union(bad_initial_rows, bad_doi_rows) # no duplicates
valid_rows <- setdiff(seq(1:nrow(data.frame)), all_bad_rows)
needed_dois <- data.frame$DI[valid_rows]
needed_names <- all_auth_names[valid_rows]

rm(bad_initial_rows)
rm(bad_doi_rows)
rm(all_bad_rows)
rm(all_auth_names)
rm(first_names)
rm(initials)

# Prep urls for crossref pull requests
base_url="https://api.crossref.org/v1/works/http://dx.doi.org/"
urls=paste0(base_url,needed_dois)

# If you have already started this process and saved an interim file, read it
# If not, create a new file to store supplemented names from crossref
# Note: This process takes a while, so you might have to pause and pick
# it back up every once in a while
if("df2_missingnames.rds" %in% list.files(paste0(folder_path, "/"))){
  new.names <- readRDS(paste0(folder_path, "/df2_missingnames.rds"))
  print("fetched file")
}else{
  new.names=data.frame(DI=needed_dois,
                       AF=data.frame$AF[valid_rows], # not initials == T
                       done=rep(0,length(needed_dois)))
  new.names$AF=as.character(new.names$AF)
  new.names$DI=as.character(new.names$DI)
}

# Determine which articles' missing names have yet to be pulled from crossref
still.to.do=which(new.names$done==0)
for(i in still.to.do){
  # if(i == 25058){ # this one had problems in highly cited, middle authors so not even important
  #   next
  # }
  # For each article, get original names and total number of authors
  orig_author_names=new.names$AF[i]
  num_authors=length(needed_names[[i]][[1]])
  
  # Pull data from crossref for article i
  json_file=urls[i]
  json_data=try(RJSONIO::fromJSON(json_file),silent=F) # RJSONIO
  
  # If the pull request works...
  if(class(json_data)!="try-error"){
    
    # And if there is actually author data...
    if(!is.null(json_data$message$author) & 
       !is.null(json_data$message$author[[1]]$family)){ # my addition, group author only in crossref
      
      # Get their names from the resulting data pull
      crossref=get.cr.auths(json_data$message$author)
      
      # If crossref has data for all authors 
      # (sometimes they only have the first author for some reason),
      # and if they are full names and not just initials...
      if(length(crossref$firsts)==num_authors & 
         !identical(crossref$firsts,toupper(crossref$firsts))){
        
        # Replace the WoS names with the crossref names in new data frame
        new.names$AF[i]=crossref$all
        print("changed by crossref")
        
      }else{
        ### INSERT CUSTOMIZED CODE HERE FROM PULLING JOURNAL WEBSITE DATA
        ### (if desired, otherwise live with data from WoS and crossref)
        
        # Inspect 'get.jneuro.auths' for an example journal-scraping function
        # Once function is customized, could be used as follows:
        
        # journal_names=get.journal.auths(json_data$message$URL)
        # new.names$AF[i]=journal_names
        # print("tried journal website")
      }
      
      # Output comparison of original WoS names and new names (if desired)
      # print(orig_author_names)
      # print(new.names$AF[i])
    }else{
      
      # If crossref didn't have author data, say that
      print("Couldn't find authors")
    }
  }else{
    
    # If crossref couldn't find the relevant DOI, say that
    # need to also close this connection somehow
    print("Couldn't find DOI")
    close_connection(urls[i])
  }
  
  # Save interim file with updated data
  if(i %% 20 == 0){ # 50 causes R to abort
    saveRDS(new.names, paste0(folder_path, "/df2_missingnames.rds"),
            compress = T)
    Sys.sleep(1)
    gc()
  }
  
  #print(showConnections(all = TRUE))
  #save(new.names, file=filename); close(filename)
  
  # Make note that you completed the pull for this article
  new.names$done[i]=1
  cat(i,"of",nrow(new.names),"\n")
  
  
  #Pause to space out pull requests
  #Sys.sleep(1)
  # time=round(runif(1,1,3),0)
  # for(t in time:1){
  #   Sys.sleep(1)
  #   # cat("Countdown:",t,"\n")
  # }
}

# get the last done
saveRDS(new.names, paste0(folder_path, "/df2_missingnames.rds"),
        compress = T)

