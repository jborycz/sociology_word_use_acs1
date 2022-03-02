# ALL DATA
soc_cap_all_mca <- soc_cap_all
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "author"] <- "AU"
soc_cap_all_mca <- soc_cap_all_mca %>% separate(AU, sep = ";", into = "AU1", remove = FALSE)
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "title"] <- "TI"
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "source_title"] <- "SO"
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "year"] <- "PY"
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "doi"] <- "DI"
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "citation_count"] <- "TC"
names(soc_cap_all_mca)[names(soc_cap_all_mca) == "reference"] <- "CR"
#  soc_cap_all_mca$CR <- paste("DOI ", soc_cap_all_mca$CR, sep="")
#  soc_cap_all_mca$CR <- gsub("; ",";DOI ",soc_cap_all_mca$CR)
#  soc_cap_all_mca$CR[soc_cap_all_mca$CR == "DOI NA" ] <- NA
soc_cap_all_mca$SR <- paste(soc_cap_all_mca$AU1,soc_cap_all_mca$PY,soc_cap_all_mca$SO,sep=", ")
soc_cap_all_mca$SR_FULL <- soc_cap_all_mca$SR
soc_cap_all_mca$DB <- "ISI"

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

### APPLY BIBLIOMETRIX TO CODE
results <- biblioAnalysis(soc_cap_all_mca_6)
summary(results, k=10, pause=F, width=130)

res <- couplingMap(soc_cap_all_mca_6, analysis="authors",field = "TI", n = 100, 
                   impact.measure="global",minfreq = 2, size = 0.5, repel = TRUE)
plot(res$map)

# Coupling map
author_coupling <- biblioNetwork(soc_cap_all_mca_6, analysis = "coupling", network = "authors", sep = ";")
author_coupling_net=networkPlot(author_coupling, n = 50, Title = "Author Coupling Network", type = "fruchterman", size.cex=TRUE, 
                size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
png(file="output/author_coupling.png",
    width=12, height=12, units="in", res=300)
  networkPlot(author_coupling, n = 50, Title = "Author Coupling Network", type = "fruchterman", size.cex=TRUE, 
            size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
dev.off()

# PERFORM MCA
cs_title <- conceptualStructure(
  subset(soc_cap_all_mca_6[1:1000,]),
  field = "TI",
  method = "MCA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 5,
  clust ="auto",
  k.max = 8,
  stemming = FALSE,
  labelsize = 12,
  documents = 5,
  graph = TRUE
)

