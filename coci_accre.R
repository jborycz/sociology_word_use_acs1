# Load libraries
library(pacman)
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
filter_low <- 200  # SPECIFY LOW FILTER FOR NUMBER OF CITATIONS. For example, filter_low = 10 will only gather DOIs with 10 or more citations.
filter_high <- 99999 # SPECIFY LOW FILTER FOR NUMBER OF CITATIONS. For example, filter_high = 20 will only gather DOIs with 20 or fewer citations.
new_folder2 <- paste0("/data/borycz_lab/acs_sociology_abstracts/","cit_",filter_low,"_",filter_high) # SPECIFY OUTPUT LOCATION
new_folder <- paste0("/data/borycz_lab/acs_sociology_abstracts/","cit_",filter_low,"_",filter_high,"/",file_name) # SPECIFY OUTPUT LOCATION
if(isFALSE(file.exists(new_folder2))){
  dir.create(paste0(new_folder2)) 
}
if(isFALSE(file.exists(new_folder))){
  dir.create(paste0(new_folder)) 
}

# Check if output files exist
tryCatch({
  output_list <- as.list(list.files(new_folder))
  output_nums <- as.numeric(gsub(".*?([0-9]+).*", "\\1", output_list))
  output_num_list <- as.list(output_nums)
  max_output <- as.numeric(max(output_nums))
  max_output_minus1 <- max_output - 1
  num_file <- output_list[[which(max_output==output_num_list)]]
  num_file_minus1 <- output_list[[which(max_output_minus1==output_num_list)]]},
  error=function(e) "error",
  warning = function(e) "warning")

# Set condition to read in previous file to continue run
if (isTRUE(length(output_num_list) == 0)){
  
  # Get citations from main paper
  result1 <- rjson::fromJSON(file = opcit)
  citing1 <- lapply(result1, function(x){x[['citation']]})
  tryCatch({
    citing1 <- unlist(citing1)
    citing1 <- as.list(strsplit(citing1, '; '))
    citing1 <- unlist(citing1)},
    error=function(e) "error",
    warning = function(e) "warning")
  
  ## Save 1st paper metadata to CSV file and dataframe
  citing1_df <- data.frame(result1)
  citing1_df$cit_level <- 0
  current_time <- gsub(":","",gsub(" ","_",Sys.time()))
  write_excel_csv(citing1_df,paste0(new_folder,"/",file_name,"_0_complete.csv"))
  m=1
} else {m <- as.numeric(max_output)
complete_test <- file.exists(paste0(new_folder,"/",file_name,"_",m,"_complete.csv"))
if (complete_test){
  citing1_df <- data.frame(read.csv(paste0(new_folder,"/",file_name,"_",m,"_complete.csv"),stringsAsFactors = FALSE))
  citing1 <- unlist(citing1_df)
  tryCatch({
    citing1 <- as.list(strsplit(citing1, '; '))
    citing1 <- unlist(citing1)},
    error=function(e) "error",
    warning = function(e) "warning")
  m<-m+1
} else{
  n <- m-1
  previous_data <- data.frame(read.csv(paste0(new_folder,"/",file_name,"_",n,"_complete.csv"),encoding="UTF-8"))
  previous_data <- previous_data$citation
  tryCatch({
    previous_data <- as.list(strsplit(previous_data, '; '))
    previous_data <- unlist(previous_data)},
    error=function(e) "error",
    warning = function(e) "warning")
  citing1_df <- data.frame(read.csv(paste0(new_folder,"/",file_name,"_",m,".csv"),encoding="UTF-8",stringsAsFactors = FALSE))
  citing1_df$volume <- as.character(citing1_df$volume)
  citing1_df$year <- as.character(citing1_df$year)
  citing1_df$citation_count <- as.character(citing1_df$citation_count)
  citing1 <- citing1_df$doi
  tryCatch({
    citing1 <- as.list(strsplit(citing1, '; '))
    citing1 <- unlist(citing1)},
    error=function(e) "error",
    warning = function(e) "warning")
  last_doi <- tail(citing1,n=1)[[1]]
  last_index <- as.numeric(tail(as.list(which(previous_data==last_doi)),n=1))+1
  citing1 <- previous_data[last_index:length(previous_data)]
}}

# Extract citation metadata 
while_test <- TRUE
stop_running <- isFALSE(is.na(citing1[1]))
while (while_test & stop_running) {
  stop_running <- isFALSE(is.na(citing1[1]))
  complete_test <- file.exists(paste0(new_folder,"/",file_name,"_",m,"_complete.csv"))
  partial_test <- file.exists(paste0(new_folder,"/",file_name,"_",m,".csv"))
  if (isFALSE(complete_test) & isFALSE(partial_test)){
    filter2_df <- data.frame()
    citing2_save <- list()
  } else if(isFALSE(complete_test) & partial_test) {
    filter2_df <- citing1_df
    citing2_save <- citing1
  } else {
    filter2_df <- data.frame()
    citing2_save <- list()
  }
  print(paste("citation_level=",m,sep=" "))
  test_list <- list()
  for (i in seq(1,length(citing1),1)){
    testit(sleep_time)
    doi <- citing1[i]
    tryCatch({
      filter2 <- rjson::fromJSON(file = paste0("https://opencitations.net/index/coci/api/v1/metadata/",doi))
      citing2 <- lapply(filter2, function(x){x[['citation']]})
      citing2 <- unlist(citing2)
      citing2 <- strsplit(citing2, '; ')
      citing2 <- unlist(citing2)
      citing2_save <- append(citing2_save,citing2)},
      error=function(e) "error",
      warning = function(e) "warning")
    if (length(citing2)>=as.numeric(filter_low) & length(citing2)<=as.numeric(filter_high)) {
      filter2_selection <- data.frame(filter2)
      filter2_selection$cit_level <- m
      filter2_df <- bind_rows(filter2_df,filter2_selection)
      test_list <- append(test_list,"YES")
      print(paste("Filter",m,i,length(citing1),doi,sep=" "))
    } else {
      test_list <- append(test_list,"NO")
      print(paste("Filter",m,i,length(citing1),sep=" "));next}
    write_excel_csv(filter2_df,paste0(new_folder,"/",file_name,"_",m,".csv"))
  }
  current_time <- gsub(":","",gsub(" ","_",Sys.time()))
  write_excel_csv(filter2_df,paste0(new_folder,"/",file_name,"_",m,"_complete.csv"))
  #  filter_num <- ifelse(round(filter_num/2^(m-1))==0,1,round(filter_num/m^(m-1)))
  #  citing1 <- citing2_save
  citing1_df <- data.frame(read.csv(paste0(new_folder,"/",file_name,"_",m,"_complete.csv"),encoding="UTF-8",stringsAsFactors = FALSE))
  citing1 <- citing1_df$citation
  tryCatch({
    citing1 <- as.list(strsplit(citing1, '; '))
    citing1 <- unlist(citing1)},
    error=function(e) "error",
    warning = function(e) "warning")
  m <- m+1
  while_test <- isTRUE("YES" %in% test_list)
}