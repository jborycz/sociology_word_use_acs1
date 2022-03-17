# Create document frequency matrix (DFM)
soc_cap_all_dfm <- dfm(tokens(soc_cap_all$title,verbose = FALSE))
soc_cap_all_dfm <- dfm_wordstem(soc_cap_all_dfm)
soc_cap_all_dfm2 <- dfm_trim(soc_cap_all_dfm, min_docfreq = 3)
diff_lim_agg_all_dfm <- dfm(tokens(diff_lim_agg_all$title,verbose = FALSE))
diff_lim_agg_all_dfm <- dfm_wordstem(diff_lim_agg_all_dfm)
diff_lim_agg_all_dfm2 <- dfm_trim(diff_lim_agg_all_dfm, min_docfreq = 3)
emergence_all_dfm <- dfm(tokens(emergence_all$title,verbose = FALSE))
emergence_all_dfm <- dfm_wordstem(emergence_all_dfm)
emergence_all_dfm2 <- dfm_trim(emergence_all_dfm, min_docfreq = 3)
# define by sparsity
#soc_cap_all_dfm2 <- dfm_trim(soc_cap_all_dfm, sparsity = 0.99)
# keep only words occurring <= 10 times and in at most 3/4 of the documents
## dfm_trim(soc_cap_all_dfm, max_termfreq = 10, max_docfreq = 0.75)
# keep only words occurring 5 times in 1000, and in 2 of 5 of documents
## dfm_trim(soc_cap_all_dfm, min_docfreq = 0.4, min_termfreq = 0.005, termfreq_type = "prop")
# keep only words occurring frequently (top 20%) and in <=2 documents
## dfm_trim(soc_cap_all_dfm, min_termfreq = 0.2, max_docfreq = 2, termfreq_type = "quantile")

# Topic model
soc_cap_lda <- textmodel_lda(soc_cap_all_dfm2, k = 180)
diff_lim_agg_lda <- textmodel_lda(diff_lim_agg_all_dfm2, k = 172)
emergence_lda <- textmodel_lda(emergence_all_dfm2, k = 135)

# Export
write_csv(soc_cap_lda$phi,"output/soc_cap/soc_cap_topic_model_phi.csv")
write_csv(soc_cap_lda$theta,"output/soc_cap/soc_cap_topic_model_theta.csv")
write_csv(diff_lim_agg_lda$phi,"output/diff_lim_agg/diff_lim_agg_topic_model_phi.csv")
write_csv(diff_lim_agg_lda$theta,"output/diff_lim_agg/diff_lim_agg_topic_model_theta.csv")
write_csv(emergence_lda$phi,"output/emergence/emergence_topic_model_phi.csv")
write_csv(emergence_lda$theta,"output/emergence/emergence_topic_model_theta.csv")

# Combine theta with original dataframe
soc_cap_topics <- data.frame(soc_cap_lda$theta)
soc_cap_topics <- data.frame(lapply(soc_cap_topics,as.numeric))
soc_cap_all_topics <- cbind(soc_cap_all, soc_cap_topics)
diff_lim_agg_topics <- data.frame(diff_lim_agg_lda$theta)
diff_lim_agg_topics <- data.frame(lapply(diff_lim_agg_topics,as.numeric))
diff_lim_agg_all_topics <- cbind(diff_lim_agg_all, diff_lim_agg_topics)
emergence_topics <- data.frame(emergence_lda$theta)
emergence_topics <- data.frame(lapply(emergence_topics,as.numeric))
emergence_all_topics <- cbind(emergence_all, emergence_topics)

# Topic model data - optimized k (Ben Horne)
soc_cap_k_topics <- read_csv("data/doc_to_topics_soc_cap_selected_preprocessed.csv")
diff_lim_agg_k_topics <- read_csv("data/doc_to_topics_diff_lim_agg_selected_preprocessed.csv")
emergence_k_topics <- read_csv("data/doc_to_topics_emergence_selected_preprocessed.csv")

## Combine k topics with the original paper citation data
left_join()


# Compute mean and std errors for all topics
## soc_cap
### Mean
soc_cap_topic_depth_mean <- soc_cap_all_topics %>%
  group_by(depth) %>%
  summarise(n=n(),across(topic1:topic50, mean))
#soc_cap_topic_depth_mean <- soc_cap_all_topics %>% gather(topic,value,topic1:topic50) %>% 
#  group_by(depth,topic) %>% mutate(mean=mean(value),sd=sd(value))
new_mean_names <- paste(colnames(soc_cap_topic_depth_mean), "_mean", sep="")
colnames(soc_cap_topic_depth_mean) <- new_mean_names
### Standard error
soc_cap_topic_depth_se <- soc_cap_all_topics %>%
  group_by(depth) %>%
  summarise(n=(1/1.96)*sqrt(n()), across(topic1:topic50, sd))
soc_cap_topic_depth_se[,-(1:2)] %<>% sapply(`/`, soc_cap_topic_depth_se[,2])
new_mean_names <- paste(colnames(soc_cap_topic_depth_se), "_se", sep="")
colnames(soc_cap_topic_depth_se) <- new_mean_names
soc_cap_topic_depth_se <- subset(soc_cap_topic_depth_se,select=-c(depth_se,n_se))
# Make long and combine
soc_cap_topic_depth_mean_long <- gather(soc_cap_topic_depth_mean, topic, mean, topic1_mean:topic50_mean, factor_key=TRUE)
soc_cap_topic_depth_se_long <- gather(soc_cap_topic_depth_se, topic2, se, topic1_se:topic50_se, factor_key=TRUE)
soc_cap_topic_depth_mean_long <- cbind(soc_cap_topic_depth_mean_long,soc_cap_topic_depth_se_long)
# Add overall mean 
soc_cap_topic_depth_mean_long <- soc_cap_topic_depth_mean_long %>% group_by(depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*1.96/sqrt(n_mean))
## diff_lim_agg
### Mean
diff_lim_agg_topic_depth_mean <- diff_lim_agg_all_topics %>%
  group_by(depth) %>%
  summarise(n=n(),across(topic1:topic50, mean))
new_mean_names <- paste(colnames(diff_lim_agg_topic_depth_mean), "_mean", sep="")
colnames(diff_lim_agg_topic_depth_mean) <- new_mean_names
### Standard error
diff_lim_agg_topic_depth_se <- diff_lim_agg_all_topics %>%
  group_by(depth) %>%
  summarise(n=(1/1.96)*sqrt(n()), across(topic1:topic50, sd))
diff_lim_agg_topic_depth_se[,-(1:2)] %<>% sapply(`/`, diff_lim_agg_topic_depth_se[,2])
new_mean_names <- paste(colnames(diff_lim_agg_topic_depth_se), "_se", sep="")
colnames(diff_lim_agg_topic_depth_se) <- new_mean_names
diff_lim_agg_topic_depth_se <- subset(diff_lim_agg_topic_depth_se,select=-c(depth_se,n_se))
# Make long and combine
diff_lim_agg_topic_depth_mean_long <- gather(diff_lim_agg_topic_depth_mean, topic, mean, topic1_mean:topic50_mean, factor_key=TRUE)
diff_lim_agg_topic_depth_se_long <- gather(diff_lim_agg_topic_depth_se, topic2, se, topic1_se:topic50_se, factor_key=TRUE)
diff_lim_agg_topic_depth_mean_long <- cbind(diff_lim_agg_topic_depth_mean_long,diff_lim_agg_topic_depth_se_long)
# Add overall mean 
diff_lim_agg_topic_depth_mean_long <- diff_lim_agg_topic_depth_mean_long %>% group_by(depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*1.96/sqrt(n_mean))
## emergence
### Mean
emergence_topic_depth_mean <- emergence_all_topics %>%
  group_by(depth) %>%
  summarise(n=n(),across(topic1:topic50, mean)) 
new_mean_names <- paste(colnames(emergence_topic_depth_mean), "_mean", sep="")
colnames(emergence_topic_depth_mean) <- new_mean_names
### Standard error
emergence_topic_depth_se <- emergence_all_topics %>%
  group_by(depth) %>%
  summarise(n=(1/1.96)*sqrt(n()), across(topic1:topic50, sd))
emergence_topic_depth_se[,-(1:2)] %<>% sapply(`/`, emergence_topic_depth_se[,2])
new_mean_names <- paste(colnames(emergence_topic_depth_se), "_se", sep="")
colnames(emergence_topic_depth_se) <- new_mean_names
emergence_topic_depth_se <- subset(emergence_topic_depth_se,select=-c(depth_se,n_se))
# Make long and combine
emergence_topic_depth_mean_long <- gather(emergence_topic_depth_mean, topic, mean, topic1_mean:topic50_mean, factor_key=TRUE)
emergence_topic_depth_se_long <- gather(emergence_topic_depth_se, topic2, se, topic1_se:topic50_se, factor_key=TRUE)
emergence_topic_depth_mean_long <- cbind(emergence_topic_depth_mean_long,emergence_topic_depth_se_long)
# Add overall mean 
emergence_topic_depth_mean_long <- emergence_topic_depth_mean_long %>% group_by(depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*1.96/sqrt(n_mean))

# Plot theme
theme_topic_line <- theme(plot.title = element_text(face="bold",size=24),
                     legend.position = "none",
                     legend.key.size = unit(1.5, "cm"),
                     legend.title = element_text(color="black",face="bold",size=36),
                     legend.text=element_text(color="black",size=32),
                     axis.text.x = element_text(color="black",size=36, angle=0),
                     strip.text.x = element_text(color="black",face="bold",size=36),
                     axis.title.x = element_text(color="black",size=48, angle=0),
                     axis.text.y = element_text(color="black",size=36, angle=0),
                     axis.title.y = element_text(color="black",size=48, angle=90))
# Plot topics 
## Correlation plots
### Depth
#### soc_cap
##### Individual
for (i in seq(1,11,1)){
  soc_cap_all_topics_1 <- subset(soc_cap_all_topics,depth==i)
  topic_correlation <- round(cor(soc_cap_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/soc_cap/topics/ggcorrplot/","topic_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
##### Mean
soc_cap_topic_depth_mean_1 <- soc_cap_topic_depth_mean[2:11,-1]
colnames(soc_cap_topic_depth_mean_1) <- seq(1,50,1)
topic_correlation <- round(cor(soc_cap_topic_depth_mean_1), 5)
p.mat <- cor_pmat(soc_cap_topic_depth_mean_1)
topic_correlation_plot <- topic_correlation %>% ggcorrplot(hc.order = TRUE,lab=TRUE,type = "lower",insig = "blank",p.mat = p.mat) + 
  labs(x="Topic Value Means", y="Topic Value Means") + theme_topic_line 
ggsave(paste0("output/soc_cap/topics/","mean_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

#### diff_lim_agg
for (i in seq(1,11,1)){
  diff_lim_agg_all_topics_1 <- subset(diff_lim_agg_all_topics,depth==i)
  topic_correlation <- round(cor(diff_lim_agg_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/diff_lim_agg/topics/ggcorrplot/","topic_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
##### Mean
diff_lim_agg_topic_depth_mean_1 <- diff_lim_agg_topic_depth_mean[2:11,-1]
colnames(diff_lim_agg_topic_depth_mean_1) <- seq(1,50,1)
topic_correlation <- round(cor(diff_lim_agg_topic_depth_mean_1), 5)
p.mat <- cor_pmat(diff_lim_agg_topic_depth_mean_1)
topic_correlation_plot <- topic_correlation %>% ggcorrplot(hc.order = TRUE,lab=TRUE,type = "lower",insig = "blank",p.mat = p.mat) + 
  labs(x="Topic Value Means", y="Topic Value Means") + theme_topic_line 
ggsave(paste0("output/diff_lim_agg/topics/","mean_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

#### emergence
for (i in seq(1,10,1)){
  emergence_all_topics_1 <- subset(emergence_all_topics,depth==i)
  topic_correlation <- round(cor(emergence_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/emergence/topics/ggcorrplot/","topic_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
##### Mean
emergence_topic_depth_mean_1 <- emergence_topic_depth_mean[2:10,-1]
colnames(emergence_topic_depth_mean_1) <- seq(1,50,1)
topic_correlation <- round(cor(emergence_topic_depth_mean_1), 5)
p.mat <- cor_pmat(emergence_topic_depth_mean_1)
topic_correlation_plot <- topic_correlation %>% ggcorrplot(hc.order = TRUE,lab=TRUE,type = "lower",insig = "blank",p.mat = p.mat) + 
  labs(x="Topic Value Means", y="Topic Value Means") + theme_topic_line 
ggsave(paste0("output/emergence/topics/","mean_correlation_depth",i,".png"), plot = topic_correlation_plot, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

### Year
#### soc_cap
year_seq <- unique(na.omit(soc_cap_all_topics[order(soc_cap_all_topics$year),]$year))
for (i in year_seq){
  soc_cap_all_topics_1 <- subset(soc_cap_all_topics,year==i)
  topic_correlation <- round(cor(soc_cap_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/soc_cap/topics/ggcorrplot/","topic_correlation_year",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
#### diff_lim_agg
year_seq <- unique(na.omit(diff_lim_agg_all_topics[order(diff_lim_agg_all_topics$year),]$year))
for (i in year_seq){
  diff_lim_agg_all_topics_1 <- subset(diff_lim_agg_all_topics,year==i)
  topic_correlation <- round(cor(diff_lim_agg_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/diff_lim_agg/topics/ggcorrplot/","topic_correlation_year",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
#### emergence
year_seq <- unique(na.omit(emergence_all_topics[order(emergence_all_topics$year),]$year))
for (i in year_seq){
  emergence_all_topics_1 <- subset(emergence_all_topics,year==i)
  topic_correlation <- round(cor(emergence_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/emergence/topics/ggcorrplot/","topic_correlation_year",i,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
}
### Author
#### soc_cap
top_authors <- data.frame(soc_cap_biblioAnalysis$Authors) %>% 
  arrange(desc(Freq)) %>% 
  mutate(AU = factor(AU, levels = unique(AU)))
soc_cap_all_topics <- soc_cap_all_topics %>% separate(author, sep = ";", into = "first_author", remove = FALSE)
j=1
for (i in top_authors$AU[1:10]){
  soc_cap_all_topics_1 <- subset(soc_cap_all_topics,first_author==i)
  topic_correlation <- round(cor(soc_cap_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/soc_cap/topics/ggcorrplot/","topic_correlation_author",j,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
  j=j+1
}
#### diff_lim_agg
top_authors <- data.frame(diff_lim_agg_biblioAnalysis$Authors) %>% 
  arrange(desc(Freq)) %>% 
  mutate(AU = factor(AU, levels = unique(AU)))
diff_lim_agg_all_topics <- diff_lim_agg_all_topics %>% separate(author, sep = ";", into = "first_author", remove = FALSE)
j=1
for (i in top_authors$AU[1:10]){
  diff_lim_agg_all_topics_1 <- subset(diff_lim_agg_all_topics,first_author==i)
  topic_correlation <- round(cor(diff_lim_agg_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/diff_lim_agg/topics/ggcorrplot/","topic_correlation_author",j,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
  j=j+1
}
#### emergence
top_authors <- data.frame(emergence_biblioAnalysis$Authors) %>% 
  arrange(desc(Freq)) %>% 
  mutate(AU = factor(AU, levels = unique(AU)))
emergence_all_topics <- emergence_all_topics %>% separate(author, sep = ";", into = "first_author", remove = FALSE)
j=1
for (i in top_authors$AU[1:10]){
  emergence_all_topics_1 <- subset(emergence_all_topics,first_author==i)
  topic_correlation <- round(cor(emergence_all_topics_1[,16:65]), 5)
  topic_correlation_plot <- ggcorrplot(topic_correlation, method = "circle")
  ggsave(paste0("output/emergence/topics/ggcorrplot/","topic_correlation_author",j,".png"), plot = topic_correlation_plot, scale = 1,
         width = 20,height = 20, dpi = 300,limitsize = TRUE)
  j=j+1
}

# Topic - geom_smooth
## Depth
### soc_cap
soc_cap_lda_terms <- data.frame(terms(soc_cap_lda))
soc_cap_lda_terms_long <- gather(soc_cap_lda_terms, topic, words, topic1:topic50, factor_key=TRUE)
soc_cap_term_list <- soc_cap_lda_terms_long %>% group_by(topic) %>% mutate(all_names = paste(words, collapse = " "))
soc_cap_term_list <- unique(subset(soc_cap_term_list,select=-c(words)))
#soc_cap_all_topics_long <- gather(soc_cap_all_topics, topic, topic_value, topic1:topic50, factor_key=TRUE)
for (i in seq(1,50,1)){
#Depth
  j=i+16
  temp <- soc_cap_all_topics[2:nrow(soc_cap_all_topics),c(15,j)]
  colnames(temp) <- c("depth","topic")
topic_plot1 <- ggplot(temp) +
#  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
  geom_smooth(mapping=aes(x = as.numeric(depth),y = as.numeric(topic)), method="loess", se = TRUE) +
#  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
  labs(title=soc_cap_term_list$all_names[i], x="Depth", 
       y="Topic Value",color="",
       fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
# Year
temp <- soc_cap_all_topics[,c(7,15,j)]
colnames(temp) <- c("year","depth","topic")
topic_plot2 <- ggplot(temp) +
  #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
  geom_smooth(mapping=aes(x = as.numeric(as.character(year)),y = as.numeric(topic)), method="loess",se = TRUE) +
  #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
  labs(title=soc_cap_term_list$all_names[i], x="Year", 
       y="Topic Value",color="",
       fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
  theme_classic() + theme_topic_line
topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                              labels = c("Depth","Year"))
ggsave(paste0("output/soc_cap/topics/geom_smooth/","topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
       width = 30,height = 15, dpi = 300,limitsize = TRUE)
}
### diff_lim_agg
diff_lim_agg_lda_terms <- data.frame(terms(diff_lim_agg_lda))
diff_lim_agg_lda_terms_long <- gather(diff_lim_agg_lda_terms, topic, words, topic1:topic50, factor_key=TRUE)
diff_lim_agg_term_list <- diff_lim_agg_lda_terms_long %>% group_by(topic) %>% mutate(all_names = paste(words, collapse = " "))
diff_lim_agg_term_list <- unique(subset(diff_lim_agg_term_list,select=-c(words)))
for (i in seq(1,50,1)){
  #Depth
  j=i+16
  temp <- diff_lim_agg_all_topics[2:nrow(diff_lim_agg_all_topics),c(15,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- ggplot(temp) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_smooth(mapping=aes(x = as.numeric(depth),y = as.numeric(topic)), method="loess", se = TRUE) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=diff_lim_agg_term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  # Year
  temp <- diff_lim_agg_all_topics[,c(7,15,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot2 <- ggplot(temp) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_smooth(mapping=aes(x = as.numeric(as.character(year)),y = as.numeric(topic)), method="loess",se = TRUE) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=diff_lim_agg_term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                                labels = c("Depth","Year"))
  ggsave(paste0("output/diff_lim_agg/topics/geom_smooth/","topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
         width = 30,height = 15, dpi = 300,limitsize = TRUE)
}
### emergence
emergence_lda_terms <- data.frame(terms(emergence_lda))
emergence_lda_terms_long <- gather(emergence_lda_terms, topic, words, topic1:topic50, factor_key=TRUE)
emergence_term_list <- emergence_lda_terms_long %>% group_by(topic) %>% mutate(all_names = paste(words, collapse = " "))
emergence_term_list <- unique(subset(emergence_term_list,select=-c(words)))
#emergence_all_topics_long <- gather(emergence_all_topics, topic, topic_value, topic1:topic50, factor_key=TRUE)
for (i in seq(1,50,1)){
  #Depth
  j=i+16
  temp <- emergence_all_topics[2:nrow(emergence_all_topics),c(15,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- ggplot(temp) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_smooth(mapping=aes(x = as.numeric(depth),y = as.numeric(topic)), method="loess",se = TRUE) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=emergence_term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  # Year
  temp <- emergence_all_topics[,c(7,15,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot2 <- ggplot(temp) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_smooth(mapping=aes(x = as.numeric(as.character(year)),y = as.numeric(topic)), method="loess",se = TRUE) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=emergence_term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                                labels = c("Depth","Year"))
  ggsave(paste0("output/emergence/topics/geom_smooth/","topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
         width = 30,height = 15, dpi = 300,limitsize = TRUE)
}

# Topic - geom_bar
## Depth
### soc_cap
for (i in seq(1,50,1)){
  j=i+16
  temp <- soc_cap_all_topics[,c(15,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- temp[-1,] %>% group_by(depth) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(depth),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                  ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=soc_cap_term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
## Year
  temp <- soc_cap_all_topics[,c(7,15,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot2 <- temp[-1,] %>% group_by(year) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(year),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=soc_cap_term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                                labels = c("Depth","Year"))
  ggsave(paste0("output/soc_cap/topics/geom_bar/","bar_topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
         width = 30,height = 15, dpi = 300,limitsize = TRUE)
}
### diff_lim_agg
for (i in seq(1,50,1)){
  j=i+16
  temp <- diff_lim_agg_all_topics[,c(15,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- temp[-1,] %>% group_by(depth) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(depth),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=diff_lim_agg_term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  ## Year
  temp <- diff_lim_agg_all_topics[,c(7,15,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot2 <- temp[-1,] %>% group_by(year) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(year),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=diff_lim_agg_term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                                labels = c("Depth","Year"))
  ggsave(paste0("output/diff_lim_agg/topics/geom_bar/","bar_topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
         width = 30,height = 15, dpi = 300,limitsize = TRUE)
}
### emergence
for (i in seq(1,50,1)){
  j=i+16
  temp <- emergence_all_topics[,c(15,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- temp[-1,] %>% group_by(depth) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(depth),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=emergence_term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  ## Year
  temp <- emergence_all_topics[,c(7,15,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot2 <- temp[-1,] %>% group_by(year) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)/sqrt(n()),n=n()) %>% ggplot(mapping=aes(x = as.numeric(year),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=emergence_term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                                labels = c("Depth","Year"))
  ggsave(paste0("output/emergence/topics/geom_bar/","bar_topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
         width = 30,height = 15, dpi = 300,limitsize = TRUE)
}

# Mean and standard error line plot
## soc_cap
soc_cap_topic_means <- ggplot(soc_cap_topic_depth_mean_long,aes(x=depth_mean, y=mean-0.02, color=topic)) +
  geom_area(aes(x=depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_line() + geom_errorbar(aes(ymin=mean-0.02-se, ymax=mean-0.02+se),color="black", width=0.15,position=position_dodge(0.05)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(-0.01,0.01)) + theme_classic() + theme_topic_line
ggsave(paste0("output/soc_cap/topics/","soc_cap_topic_means.png"), plot = soc_cap_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
 ## diff_lim_agg
diff_lim_agg_topic_means <- ggplot(diff_lim_agg_topic_depth_mean_long,aes(x=depth_mean, y=mean-0.02, color=topic)) +
  geom_area(aes(x=depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_line() + geom_errorbar(aes(ymin=mean-0.02-se, ymax=mean-0.02+se),color="black", width=0.15,position=position_dodge(0.05)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(-0.01,0.04)) + theme_classic() + theme_topic_line
ggsave(paste0("output/diff_lim_agg/topics/","diff_lim_agg_topic_means.png"), plot = diff_lim_agg_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
 ## emergence
emergence_topic_means <- ggplot(emergence_topic_depth_mean_long,aes(x=depth_mean, y=mean-0.02, color=topic)) +
  geom_area(aes(x=depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_line() + geom_errorbar(aes(ymin=mean-0.02-se, ymax=mean-0.02+se),color="black", width=0.15,position=position_dodge(0.05)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(-0.01,0.03)) + theme_classic() + theme_topic_line
ggsave(paste0("output/emergence/topics/","emergence_topic_means.png"), plot = emergence_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
