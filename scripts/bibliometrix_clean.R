# Create ISI/Web of Science dataframe for bibliometrix
soc_cap_all_isi <- soc_cap_all
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "author"] <- "AU"
soc_cap_all_isi <- soc_cap_all_isi %>% separate(AU, sep = ";", into = "AU1", remove = FALSE)
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "title"] <- "TI"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "source_title"] <- "SO"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "year"] <- "PY"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "doi"] <- "DI"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "citation_count"] <- "TC"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "reference"] <- "CR"
#  soc_cap_all_isi$CR <- paste("DOI ", soc_cap_all_isi$CR, sep="")
#  soc_cap_all_isi$CR <- gsub("; ",";DOI ",soc_cap_all_isi$CR)
#  soc_cap_all_isi$CR[soc_cap_all_isi$CR == "DOI NA" ] <- NA
soc_cap_all_isi$SR <- paste(soc_cap_all_isi$AU1,soc_cap_all_isi$PY,soc_cap_all_isi$SO,sep=", ")
soc_cap_all_isi$SR_FULL <- soc_cap_all_isi$SR
soc_cap_all_isi$DB <- "ISI"

# Save 
soc_cap_biblioAnalysis <- biblioAnalysis(soc_cap_all_isi)
