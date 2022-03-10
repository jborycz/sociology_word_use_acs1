# ALL DATA
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

soc_cap_all_mca <- subset(soc_cap_all_mca,sample_percent=="100" | sample_percent=="20")
soc_cap_all_mca_6 <- subset(soc_cap_all_mca,depth=="6")
soc_cap_all_mca_8 <- subset(soc_cap_all_mca,depth=="8")

### TEST BIBLIOMETRIX
# Converting the loaded files into a R bibliographic dataframe
M <- convert2df("http://bibliometrix.org/datasets/joi.txt", dbsource="isi",format="plaintext")
metaTagExtraction(M, Field = "SR", sep = ";", aff.disamb = TRUE)
# Co-citation journal
M=metaTagExtraction(M,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, 
                size=15, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
# Cocitation article
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, 
                size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
res <- couplingMap(M, analysis = "authors", field = "TI", n = 100, impact.measure="global",
                   minfreq = 1, size = 0.5, repel = TRUE)
plot(res$map)


# PERFORM MCA
cs_title <- conceptualStructure(
  subset(soc_cap_all_isi),
  field = "TI",
  method = "MCA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 3,
  clust ="auto",
  k.max = 8,
  stemming = FALSE,
  labelsize = 12,
  documents = 2,
  graph = TRUE
)

