# Get citations: https://stackoverflow.com/questions/55064193/retrieve-citations-of-a-journal-paper-using-r
# Load libraries
library(pacman)
#remotes::install_github("ropensci/rcrossref")
pacman::p_load(rjson,rcrossref,dplyr,tidyr,tibble,R.utils,data.table,stringr,readr)

# Get papers citing "DIFFUSION-LIMITED AGGREGATION, A KINETIC CRITICAL PHENOMENON"
  opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.1103/PhysRevLett.47.1400"
  new_folder <- "output/diff_lim_agg"
  sink(paste0(new_folder,"/output.txt"))
# Get papers citing "Social capital, intellectual capital, and the organizational advantage"
  #opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.2307/259373"
  #new_folder <- "output/soc_cap"
# Get papers citing "Emergence of scaling in random networks "
  #opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.1126/science.286.5439.509"
  #new_folder <- "output/emergence"

# Create folder for output files
dir.create(paste0(new_folder))

# Citation deepness 1
## Extract DOI of citations
result1 <- rjson::fromJSON(file = opcit)
citing1 <- lapply(result1, function(x){
  x[['citing']]
})
citing1_unlisted <- unlist(citing1)

## Extract reference information on citations from crossref 
crossref_load_df1 <- data.frame()
for (i in seq(1,length(citing1_unlisted),100)){
  test_i <- length(citing1_unlisted)-i
  if (test_i<100){
    tryCatch({
      citing1_crossref <- rcrossref::cr_works(citing1_unlisted[i])},
      error=function(e) "error",
      warning = function(e) "warning")
    citing1_crossref_100 <- data.frame(citing1_crossref$data)
    crossref_load_df1 <- bind_rows(crossref_load_df1,citing1_crossref_100)
    print(paste("Crossref load",1,i,length(citing1_unlisted),sep=" "))
  } else {
  k=i+100
  tryCatch({
    citing1_crossref <- rcrossref::cr_works(citing1_unlisted[i:k])},
    error=function(e) "error",
    warning = function(e) "warning")
  citing1_crossref_100 <- data.frame(citing1_crossref$data)
  crossref_load_df1 <- bind_rows(crossref_load_df1,citing1_crossref_100)
  print(paste("Crossref load",1,i,k,length(citing1_unlisted),sep=" "))
  }
}
## Delete unnecessary list columns and change remaining to characters
crossref_load_df1.backup <- crossref_load_df1
col_select <- colnames(crossref_load_df1)
nope <- c("assertion","link","funder","license","update_to","url","update.policy","archive","abstract","reference")
col_select <- col_select[col_select %in% nope  == FALSE]
crossref_load_df1 <- subset(crossref_load_df1,select=col_select)
print(paste("Unnecessary columns deleted"))

## Clean author and reference columns
#for (i in seq(1,nrow(crossref_load_df1),1)) {
#  for (j in seq(1,length(crossref_load_df1[i,]$author),1)) {
#    authors <- data.frame(crossref_load_df1[i,]$author)
#    authors <- data.frame(authors)
#    authors <- paste(authors$family, authors$given, sep=",")
#    crossref_load_df1[i,]$author[[j]] <- str_c(authors, collapse = ";")
#    print(paste("Clean authors",i,nrow(crossref_load_df2),sep=" "))
#  }
#}
#for (i in seq(1,nrow(crossref_load_df1),1)) {
#  for (j in seq(1,length(crossref_load_df1[i,]$reference),1)) {
#    reference <- data.frame(crossref_load_df1[i,]$reference)
#    reference <- data.frame(na.omit(reference$DOI));colnames(reference)<-"V1"
#    crossref_load_df1[i,]$reference[[j]] <- str_c(reference$V1, collapse = ";")
#    print(paste("Clean references",i,nrow(crossref_load_df2),sep=" "))
#  }
#}

## Export citations to CSV
crossref_load_df1$author <- sapply(crossref_load_df1$author, function(x) paste0(unlist(x), collapse = ";"))
current_time <- gsub(":","",gsub(" ","_",Sys.time()))
write_excel_csv(crossref_load_df1,paste0(new_folder,"/all_crossref_data_1_",current_time,".csv"))
# crossref_load_df1 <- read.csv(paste0(new_folder,"/all_crossref_data_1.csv"))

## Limit to articles with >=5 citations
citing1_crossref_df <- data.frame()
for (i in seq(1,nrow(crossref_load_df1),1)){
  citing1_crossref_row <- crossref_load_df1[i,]
  if (citing1_crossref_row$type=="journal-article" & as.numeric(citing1_crossref_row$is.referenced.by.count)>=100){
    tryCatch({
      citing1_crossref_df <- bind_rows(citing1_crossref_df,citing1_crossref_row)},
      error=function(e) "error",
      warning = function(e) "warning")
  } else {print(paste("Crossref filter",1,i,nrow(crossref_load_df1),sep=" ")); next
    }
  print(paste("Crossref filter",1,i,nrow(crossref_load_df1),citing1_crossref_row$doi,sep=" "))
}

## Write reference data to CSV
citing1_crossref_df$cit_level <- 1
citing1_crossref_df.backup <- citing1_crossref_df
current_time <- gsub(":","",gsub(" ","_",Sys.time()))
write_excel_csv(citing1_crossref_df,paste0(new_folder,"/filtered_crossref_data_1_",current_time,".csv"))

## Take filtered DOIs from crossref table
citing1_filtered <- citing1_crossref_df$doi

# Citation deepness 2
j=2
while (length(citing1_filtered)>0) {
  print(paste("citation_level=",j,sep=" "))
  temp_df <- data.frame()
## Extract DOI of citations
for (i in seq(1,length(citing1_filtered),1)){
  doi <- citing1_filtered[[i]]
  opcit1 <- paste0("https://opencitations.net/index/coci/api/v1/citations/",doi)
  tryCatch({withTimeout(result2 <- rjson::fromJSON(file = opcit1), timeout = 60)},
           TimeoutException = function(e) {print("Timeout of 60 seconds reached. Function will not be completely run.")},      
           error=function(e) "error",
           warning = function(e) "warning")
  citing2_temp <- lapply(result2, function(x){
    x[['citing']]
  })
  citing2_temp <- data.frame(unlist(citing2_temp))
  citing2_temp$cited_doi <- doi
  temp_df <- bind_rows(temp_df,citing2_temp)
  print(paste("DOI",j,i,length(citing1_filtered),sep=" "))
}
print(paste("DOI",nrow(temp_df),j,"done", sep=" "))
citing2 <- temp_df

## Extract reference information on citations from crossref 
crossref_load_df2 <- data.frame()
for (i in seq(1,nrow(citing2)-100,100)){
  test_i <- nrow(citing2)-i
  if (test_i<100){
    tryCatch({
      citing2_crossref <- rcrossref::cr_works(citing2[i])},
      error=function(e) "error",
      warning = function(e) "warning")
    citing2_crossref_100 <- data.frame(citing2_crossref$data)
    crossref_load_df2 <- bind_rows(crossref_load_df2,citing2_crossref_100)
    print(paste("Crossref load",1,i,length(citing2_unlisted),sep=" "))
  } else {
  k=i+100
  tryCatch({
   citing2_crossref <- rcrossref::cr_works(citing2[i:k,1])},
   error=function(e) "error",
   warning = function(e) "warning")
  citing2_crossref_100 <- data.frame(citing2_crossref$data)
  crossref_load_df2 <- bind_rows(crossref_load_df2,citing2_crossref_100)
  print(paste("Crossref load",j,i,i+100,nrow(citing2),sep=" "))
  }
}
citing2_crossref <- rcrossref::cr_works(citing2)

##  Delete unnecessary list columns and change remaining to characters
crossref_load_df2.backup <- crossref_load_df2
col_select <- colnames(crossref_load_df2)
nope <- c("assertion","link","funder","license","update_to","url","update.policy","archive","abstract","reference")
col_select <- col_select[col_select %in% nope  == FALSE]
crossref_load_df2 <- subset(crossref_load_df2,select=col_select)
print(paste("Unnecessary columns deleted"))

## Clean author and reference columns
#for (i in seq(1,nrow(crossref_load_df2),1)) {
#  for (n in seq(1,length(crossref_load_df2[i,]$author),1)) {
#    authors <- data.frame(crossref_load_df2[i,]$author)
#    authors <- data.frame(authors)
#    authors <- paste(authors$family, authors$given, sep=",")
#    crossref_load_df2[i,]$author[[n]] <- str_c(authors, collapse = ";")
#    print(paste("Clean authors",i,nrow(crossref_load_df2),sep=" "))
#  }
#}
#for (i in seq(1,nrow(crossref_load_df2),1)) {
#  for (n in seq(1,length(crossref_load_df2[i,]$reference),1)) {
#    reference <- data.frame(crossref_load_df2[i,]$reference)
#    reference <- data.frame(na.omit(reference$DOI));colnames(reference)<-"V1"
#    crossref_load_df2[i,]$reference[[n]] <- str_c(reference$V1, collapse = ";")
#    print(paste("Clean references",i,sep=" "))
#  }
#}
crossref_load_df2$author <- sapply(crossref_load_df2$author, function(x) paste0(unlist(x), collapse = ";"))
current_time <- gsub(":","",gsub(" ","_",Sys.time()))
write_excel_csv(crossref_load_df2,paste0(new_folder,"/all_crossref_data_",j,"_",curent_time,".csv"))

## Limit to articles with >=5 citations
citing2_crossref_df <- data.frame()
for (i in seq(1,nrow(crossref_load_df),1)){
  citing2_crossref_row <- crossref_load_df2[i,]
  citing2_crossref_row$cited_doi <- citing2$cited_doi[i]
  if (citing2_crossref_row$type=="journal-article" & as.numeric(citing2_crossref_row$is.referenced.by.count)>=100){
    tryCatch({
      citing2_crossref_df <- bind_rows(citing2_crossref_df,citing2_crossref_row)},
      error=function(e) "error",
      warning = function(e) "warning")
  } else {print(paste("Crossref filter",j,i,sep=" "));next
        }
  print(paste("Crossref filter",j,i,citing2_crossref_row$doi,sep=" "))
}
print(paste("Crossref",j,"done", sep=" "))

# Write reference data to CSV
citing2_crossref_df$cit_level <- j
citing2_crossref_df.backup <- citing2_crossref_df
current_time <- gsub(":","",gsub(" ","_",Sys.time()))
write_excel_csv(citing2_crossref_df,paste0(new_folder,"/filtered_crossref_data_",j,"_",current_time,".csv"))
citing1_filtered <- citing2_crossref_df$doi
j=j+1
} 
sink()


