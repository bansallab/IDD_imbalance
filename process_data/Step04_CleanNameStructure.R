source("HelperFunctions.R")
library(pbmcapply);library(stringr)
library(RJSONIO);library(textclean)
library(tidyverse); library(stringi)

# change to appropriate field definition
folder_path <- "highlycited"
# Load in all-journal dataset from step 3
if("df4_articledata_cleannames.rds" %in% list.files(paste0(folder_path, "/"))){
  article.data <- readRDS(paste0(folder_path, "/df4_articledata_cleannames.rds"))
  print("fetched file")
}else{
  article.data <- readRDS(file = paste0(folder_path, "/df3_articledata.rds"))
  
  #1/4 --> '
  # Mi 1/2 --> Muller
  # F. {e. }O. --> Francois
  # } --> Z
  # u 3/4OE-" 1/4 -> a
  # u 3/4~-" 1/4 -> e
  # ^ --> S
  article.data <- article.data %>% 
    mutate(AF = stri_unescape_unicode(gsub("&#x0", "\\u", gsub("(&#x0)(.{4})(;)", "\\1\\2", AF), fixed = T)),
           AF = gsub("\\(|\\)|\\?|&|#|~|`|*|_|^-|-and|>|^\\.", "",
                     gsub("\\.\\.", "\\.",
                          gsub("\\+", "A", 
                               gsub(" -", " ",
                                    gsub("\\$\\$", "c", # one case
                                         gsub("\\$eacute\\$", "e",
                                              gsub("\\$ccedil\\$", "e",
                                                   gsub("@", "l", # one case
                                                        gsub("1", "i",
                                                             gsub("1/4", "\'",
                                                                  gsub("\\}|\\{", "Z",
                                                                       gsub("\\^", "S",
                                                                            gsub("F. \\{e. \\}O.", "Francois", # one case
                                                                                 gsub("Mi 1/2", "Muller", # couple cases
                                                                                      gsub("u 3/4OE-\" 1/4", "a",
                                                                                           gsub("u 3/4~-\" 1/4", "e",
                                                                                                stri_trans_general(str = AF, id = "Latin-ASCII"))))))))))))))))))
  
}


# Find number of authors in each article
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1 
# I think this is fixed since I added space to python pull code, essential that this spacing is correct
# Find number of first-/last-name delineations in each author list
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Find articles where at least one author did not have delineated first-/last-name
missing.comma=which(num.auths>num.commas)
r=0; num.missing=length(missing.comma)

# For articles with missing delineation...
for(i in missing.comma){
  
  # Pull name data from crossref to get delineated first-/last-names
  
  try({
    # may get an error here due to invalid UTF
    delineated.names=add.miss.comma(i,article.data$AF,article.data$DI)
    # this uses crossref need to filter & close again
    #print(showConnections(all = TRUE))
    
    # Enter the newly formatted names into dataset
    # Note: gsub call just adds a space (" ") for names with no provided first name
    article.data$AF[i]=gsub(",;",", ;",delineated.names)
  })
  
  if(r %% 100 == 0){ # save every 100 in case gets hung up
    saveRDS(article.data, file = paste0(folder_path, "/df4_articledata_cleannames.rds"))
  }
  
  # Iterate and provide progress update
  r=r+1; cat(r,"of",num.missing,"\n")
  
  # Pause to space out pull requests
  # time=round(runif(1,1,3),0)
  # for(t in time:1){
  #   Sys.sleep(1)
  #   # cat("Countdown:",t,"\n")
  # }
}

# Recalculate author counts and delineations
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Check whether any articles are still missing first/last name delineation
which(num.auths>num.commas)

# Now look for instances where there are more comma delineations than authors
# (this is usually because WoS styles "John Doe Jr." as "Doe, John, Jr.")
extra.comma=which(num.auths<num.commas)

# Remove those extra commas to get clean first-/last-name delineation
new.ec.names=unlist(lapply(article.data$AF[extra.comma],rm.extra.comma))
# Replace instances in dataset
article.data$AF[extra.comma]=new.ec.names

# Recalculate author counts and delineations
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Check whether any articles still have an extra comma
which(num.auths!=num.commas)

# Save cleaned up dataset
saveRDS(article.data, file=paste0(folder_path, "/df4_articledata_cleannames.rds"))

