# Get citations: https://stackoverflow.com/questions/55064193/retrieve-citations-of-a-journal-paper-using-r
# Load libraries
library(pacman)
#remotes::install_github("ropensci/rcrossref")
pacman::p_load(rjson,dplyr,tidyr,tibble,R.utils,data.table,stringr,readr)

# SELECT WHICH PAPER YOU WANT TO COLLECT DOIs FROM
## Get papers citing "DIFFUSION-LIMITED AGGREGATION, A KINETIC CRITICAL PHENOMENON"
  opcit <- "https://opencitations.net/index/coci/api/v1/metadata/10.1103/PhysRevLett.47.1400"
## Get papers citing "Social capital, intellectual capital, and the organizational advantage"
#  opcit <- "https://opencitations.net/index/coci/api/v1/metadata/10.2307/259373"
## Get papers citing "Emergence of scaling in random networks "
  #opcit <- "https://opencitations.net/index/coci/api/v1/metadata/10.1126/science.286.5439.509"

# SET UP COLLECTION
testit <- function(x){p1 <- proc.time();Sys.sleep(x);proc.time() - p1} # SLEEP TIMER
sleep_time <- 0.0 # SPECIFY SLEEP TIME BETWEEN API CALLS
file_name <- "diff_lim_agg" # SPECIFY DESIRED FILENAME TO EXPORT FILES
new_folder <- "output/coci" # SPECIFY OUTPUT LOCATION
dir.create(paste0(new_folder)) # CREATE OUTPUT FOLDER
filter_num <- 50 # SPECIFY FILTER FOR NUMBER OF CITATIONS. For example, filter_num = 10 will only gather DOIs with 10 or more citations.

# Get citations from main paper
result1 <- rjson::fromJSON(file = opcit)
citing1 <- lapply(result1, function(x){x[['citation']]})
citing1 <- as.list(strsplit(citing1[[1]], '; '))
citing1 <- unlist(citing1)

## Filter DOI by number of citations
while_test <- TRUE
citing1_df <- data.frame(result1)
citing1_df <- subset(citing1_df,select=-c(citation))
citing1_df$cit_level <- 0
current_time <- gsub(":","",gsub(" ","_",Sys.time()))
write.csv(citing1_df,file=paste0(new_folder,"/",file_name,"_0_",current_time,".csv"))

m=1
while (while_test) {
  filter2_df <- data.frame()
  citing2_save <- list()
  print(paste("citation_level=",m,sep=" "))
  test_list <- list()
for (i in seq(1,length(citing1),1)){
    testit(sleep_time)
    doi <- citing1[20]
    tryCatch({
      filter2 <- rjson::fromJSON(file = paste0("https://opencitations.net/index/coci/api/v1/metadata/",doi))},
      error=function(e) "error",
      warning = function(e) "warning")
    citing2 <- lapply(filter2, function(x){x[['citation']]})
    citing2 <- unlist(citing2)
    citing2 <- strsplit(citing2, '; ')
    citing2 <- unlist(citing2)
    citing2_save <- append(citing2_save,citing2)
    if (length(citing2)>=as.numeric(filter_num)) {
      filter2_selection <- data.frame(filter2)
      filter2_selection <- subset(filter2_selection,select=-c(citation))
      filter2_selection$cit_level <- m
      filter2_df <- bind_rows(filter2_df,filter2_selection)
      test_list <- append(test_list,"YES")
      print(paste("Filter",m,i,length(citing1),doi,sep=" "))
    } else {
      test_list <- append(test_list,"NO")
      print(paste("Filter",m,i,length(citing1),sep=" "));next}
}
  current_time <- gsub(":","",gsub(" ","_",Sys.time()))
  write.csv(filter2_df,file=paste0(new_folder,"/",file_name,"_",m,"_",current_time,".csv"))
  filter_num <- ifelse(round(filter_num/2^(m-1))==0,1,round(filter_num/m^(m-1)))
  m=m+1
  citing1 <- citing2_save
  while_test <- isTRUE("YES" %in% test_list)
}
