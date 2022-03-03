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
soc_cap_lda <- textmodel_lda(soc_cap_all_dfm2, k = 50)
diff_lim_agg_lda <- textmodel_lda(diff_lim_agg_all_dfm2, k = 50)
emergence_lda <- textmodel_lda(emergence_all_dfm2, k = 50)

# Export
write.csv(soc_cap_lda$phi,"output/soc_cap_topic_model_phi.csv")
write.csv(soc_cap_lda$theta,"output/soc_cap_topic_model_theta.csv")
write.csv(diff_lim_agg_lda$phi,"output/diff_lim_agg_topic_model_phi.csv")
write.csv(diff_lim_agg_lda$theta,"output/diff_lim_agg_topic_model_theta.csv")
write.csv(emergence_lda$phi,"output/emergence_topic_model_phi.csv")
write.csv(emergence_lda$theta,"output/emergence_topic_model_theta.csv")

# Combine theta with original dataframe
soc_cap_topics <- data.frame(soc_cap_lda$theta)
soc_cap_topics <- data.frame(lapply(soc_cap_topics,as.numeric))
soc_cap_all_topics <- cbind(soc_cap_all, soc_cap_topics)

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
## Depth
soc_cap_lda_terms <- data.frame(terms(soc_cap_lda))
soc_cap_lda_terms_long <- gather(soc_cap_lda_terms, topic, words, topic1:topic50, factor_key=TRUE)
term_list <- soc_cap_lda_terms_long %>% group_by(topic) %>% mutate(all_names = paste(words, collapse = " "))
term_list <- unique(subset(term_list,select=-c(words)))
#soc_cap_all_topics_long <- gather(soc_cap_all_topics, topic, topic_value, topic1:topic50, factor_key=TRUE)
for (i in seq(1,50,1)){
#Depth
  j=i+15
  temp <- soc_cap_all_topics[,c(14,j)]
  colnames(temp) <- c("depth","topic")
topic_plot1 <- ggplot(temp) +
#  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
  geom_smooth(mapping=aes(x = as.numeric(depth),y = as.numeric(topic)), method="loess",se = FALSE) +
#  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
  labs(title=term_list$all_names[i], x="Depth", 
       y="Topic Value",color="",
       fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
# Year
temp <- soc_cap_all_topics[,c(7,14,j)]
colnames(temp) <- c("year","depth","topic")
topic_plot2 <- ggplot(temp) +
  #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
  geom_smooth(mapping=aes(x = as.numeric(as.character(year)),y = as.numeric(topic)), method="loess",se = FALSE) +
  #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
  labs(title=term_list$all_names[i], x="Year", 
       y="Topic Value",color="",
       fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
  theme_classic() + theme_topic_line
topic_plot_combo <- ggarrange(topic_plot1,topic_plot2, ncol = 2, 
                              labels = c("Depth","Year"))
ggsave(paste0("output/soc_cap/topics/","topic_plot_combo",i,".png"), plot = topic_plot_combo, scale = 1,
       width = 15,height = 15, dpi = 300,limitsize = TRUE)
}

## Depth bar
for (i in seq(1,50,1)){
  j=i+15
  temp <- soc_cap_all_topics[,c(14,j)]
  colnames(temp) <- c("depth","topic")
  topic_plot1 <- temp[-1,] %>% group_by(depth) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)) %>% ggplot(mapping=aes(x = as.numeric(depth),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                  ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=term_list$all_names[i], x="Depth", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
}

## Year bar
for (i in seq(1,50,1)){
  j=i+15
  temp <- soc_cap_all_topics[,c(7,14,j)]
  colnames(temp) <- c("year","depth","topic")
  topic_plot1 <- temp[-1,] %>% group_by(year) %>% 
    summarize(mean_topic=mean(topic),std_topic=sd(topic)) %>% ggplot(mapping=aes(x = as.numeric(year),y = as.numeric(mean_topic))) +
    #  geom_point(mapping=aes(x = as.numeric(depth), y = as.numeric(topic)),size=3) +
    geom_bar(stat="identity",fill="red") +
    geom_errorbar(aes(ymin = mean_topic - std_topic, 
                      ymax = mean_topic + std_topic),
                  size = 0.5, width = 0.5) +
    #  facet_wrap(~topic,ncol=10,labeller=term_list$all_names) + 
    labs(title=term_list$all_names[i], x="Year", 
         y="Topic Value",color="",
         fill="",caption="") + scale_x_continuous() + scale_y_continuous() + 
    theme_classic() + theme_topic_line
  ggsave(paste0("output/soc_cap/topics/year/","topic_plot_year_bar",i,".png"), plot = topic_plot1, scale = 1,
         width = 15,height = 15, dpi = 300,limitsize = TRUE)
}
