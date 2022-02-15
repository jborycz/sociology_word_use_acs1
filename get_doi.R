# Get citations: https://stackoverflow.com/questions/55064193/retrieve-citations-of-a-journal-paper-using-r
# Load libraries
library(pacman)
#remotes::install_github("ropensci/rcrossref")
pacman::p_load(rjson,dplyr,tidyr,tibble,R.utils,data.table,stringr,readr)

# Get papers citing "DIFFUSION-LIMITED AGGREGATION, A KINETIC CRITICAL PHENOMENON"
#  opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.1103/PhysRevLett.47.1400"
# Get papers citing "Social capital, intellectual capital, and the organizational advantage"
  opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.2307/259373"
# Get papers citing "Emergence of scaling in random networks "
  #opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.1126/science.286.5439.509"

# Create folder for output files
new_folder <- "/data/borycz_lab/acs_sociology_abstracts"
#dir.create(paste0(new_folder))

result1 <- rjson::fromJSON(file = opcit)
citing1 <- lapply(result1, function(x){
  x[['citing']]
})
citing1_unlisted <- unlist(citing1)

## Filter DOI by number of citations
while_test <- TRUE
citing1_df <- data.frame(citing1_unlisted)
colnames(citing1_df) <- "doi"
citing1_df$cit_level <- 1
write.csv(citing1_df,file=paste0(new_folder,"/soc_cap_doi_1.csv"))

m=2
while (while_test) {
  filter1_df <- data.frame()
  print(paste("citation_level=",m,sep=" "))
  temp_df <- data.frame()
  test_list <- list()
for (i in seq(1,nrow(citing1_df),1)){
    doi <- citing1_df[i,1]
    tryCatch({
      filter1 <- rjson::fromJSON(file = paste0("https://opencitations.net/index/coci/api/v1/citations/",doi))},
      error=function(e) "error",
      warning = function(e) "warning")
    if (length(filter1)>=10) {
      filter1_citing <- lapply(filter1, function(x){x[['citing']]})
      filter1_citing <- unlist(filter1_citing)
      filter1_selection <- data.frame(filter1_citing)
      colnames(filter1_selection) <- "doi"
      filter1_selection$cit_level <- m
      filter1_df <- bind_rows(filter1_df,filter1_selection)
      test_list <- append(test_list,"YES")
      print(paste("Filter",m,i,nrow(citing1_df),doi,sep=" "))
    } else {
      test_list <- append(test_list,"NO")
      print(paste("Filter",m,i,nrow(citing1_df),sep=" "));next}
}
  write.csv(filter1_df,file=paste0(new_folder,"/soc_cap_doi_",m,".csv"))
  m=m+1
  citing1_df <- filter1_df
  while_test <- isTRUE("YES" %in% test_list)

