# Create ISI/Web of Science dataframe for bibliometrix
## soc_cap
soc_cap_all_isi <- soc_cap_all
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "author"] <- "AU"
soc_cap_all_isi <- soc_cap_all_isi %>% separate(AU, sep = ";", into = "AU1", remove = FALSE)
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "title"] <- "TI"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "source_title"] <- "SO"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "year"] <- "PY"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "doi"] <- "DI"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "citation_count"] <- "TC"
names(soc_cap_all_isi)[names(soc_cap_all_isi) == "reference"] <- "CR"
soc_cap_all_isi$SR <- paste(soc_cap_all_isi$AU1,soc_cap_all_isi$PY,soc_cap_all_isi$SO,sep=", ")
soc_cap_all_isi$SR_FULL <- soc_cap_all_isi$SR
soc_cap_all_isi$DB <- "ISI"

## diff_lim_agg
diff_lim_agg_all_isi <- diff_lim_agg_all
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "author"] <- "AU"
diff_lim_agg_all_isi <- diff_lim_agg_all_isi %>% separate(AU, sep = ";", into = "AU1", remove = FALSE)
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "title"] <- "TI"
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "source_title"] <- "SO"
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "year"] <- "PY"
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "doi"] <- "DI"
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "citation_count"] <- "TC"
names(diff_lim_agg_all_isi)[names(diff_lim_agg_all_isi) == "reference"] <- "CR"
diff_lim_agg_all_isi$SR <- paste(diff_lim_agg_all_isi$AU1,diff_lim_agg_all_isi$PY,diff_lim_agg_all_isi$SO,sep=", ")
diff_lim_agg_all_isi$SR_FULL <- diff_lim_agg_all_isi$SR
diff_lim_agg_all_isi$DB <- "ISI"

## emergence
emergence_all_isi <- emergence_all
names(emergence_all_isi)[names(emergence_all_isi) == "author"] <- "AU"
emergence_all_isi <- emergence_all_isi %>% separate(AU, sep = ";", into = "AU1", remove = FALSE)
names(emergence_all_isi)[names(emergence_all_isi) == "title"] <- "TI"
names(emergence_all_isi)[names(emergence_all_isi) == "source_title"] <- "SO"
names(emergence_all_isi)[names(emergence_all_isi) == "year"] <- "PY"
names(emergence_all_isi)[names(emergence_all_isi) == "doi"] <- "DI"
names(emergence_all_isi)[names(emergence_all_isi) == "citation_count"] <- "TC"
names(emergence_all_isi)[names(emergence_all_isi) == "reference"] <- "CR"
emergence_all_isi$SR <- paste(emergence_all_isi$AU1,emergence_all_isi$PY,emergence_all_isi$SO,sep=", ")
emergence_all_isi$SR_FULL <- emergence_all_isi$SR
emergence_all_isi$DB <- "ISI"

# Save 
soc_cap_biblioAnalysis <- biblioAnalysis(soc_cap_all_isi)
diff_lim_agg_biblioAnalysis <- biblioAnalysis(diff_lim_agg_all_isi)
emergence_biblioAnalysis <- biblioAnalysis(emergence_all_isi)

#summary(soc_cap_biblioAnalysis)
#summary(diff_lim_agg_biblioAnalysis)
summary(emergence_biblioAnalysis)

# Plot author coupling network
## soc_cap
soc_cap_author_coupling <- biblioNetwork(soc_cap_all_isi, analysis = "coupling", network = "authors", sep = ";")
png(file="output/soc_cap/soc_cap_author_coupling.png",
    width=12, height=12, units="in", res=300)
networkPlot(soc_cap_author_coupling, n = 50, Title = "Author Coupling Network", type = "fruchterman", size.cex=TRUE, 
            size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
dev.off()

# diff_lim_agg
diff_lim_agg+author_coupling <- biblioNetwork(diff_lim_agg_all_isi, analysis = "coupling", network = "authors", sep = ";")
png(file="output/diff_lim_agg/diff_lim_agg_author_coupling.png",
    width=12, height=12, units="in", res=300)
networkPlot(diff_lim_agg+author_coupling, n = 50, Title = "Author Coupling Network", type = "fruchterman", size.cex=TRUE, 
            size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
dev.off()

# emergence
emergence_author_coupling <- biblioNetwork(emergence_all_isi, analysis = "coupling", network = "authors", sep = ";")
png(file="output/emergence/emergence_author_coupling.png",
    width=12, height=12, units="in", res=300)
networkPlot(emergence_author_coupling, n = 50, Title = "Author Coupling Network", type = "fruchterman", size.cex=TRUE, 
            size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
dev.off()

