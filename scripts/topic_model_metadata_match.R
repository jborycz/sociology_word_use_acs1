# Topic model data - optimized k (Ben Horne)
## Read files
### Forwards
soc_cap_topics_forwards <- read_csv("data/metadata_match/doc_to_topics_soc_cap_matchwithmeta_13.csv")
diff_lim_agg_topics_forwards <- read_csv("data/metadata_match/doc_to_topics_diff_lim_agg_matchwithmeta_12.csv")
emergence_topics_forwards <- read_csv("data/metadata_match/doc_to_topics_emergence_matchwithmeta_12.csv")
names(soc_cap_topics_forwards)[names(soc_cap_topics_forwards) == "id"] <- "doi"
names(diff_lim_agg_topics_forwards)[names(diff_lim_agg_topics_forwards) == "id"] <- "doi"
names(emergence_topics_forwards)[names(emergence_topics_forwards) == "id"] <- "doi"

## Join with metadata
soc_cap_all_topics_forwards <- left_join(soc_cap_topics_forwards,soc_cap_all,by="doi")
diff_lim_agg_all_topics_forwards <- left_join(diff_lim_agg_topics_forwards,diff_lim_agg_all,by="doi")
emergence_all_topics_forwards <- left_join(emergence_topics_forwards,emergence_all,by="doi")

## Filter out repeats
duplicates_soc_cap <- duplicated(soc_cap_all_topics_forwards[1]); length(duplicates_soc_cap[duplicates_soc_cap== TRUE])
soc_cap_all_topics_forwards_dedup <- data.frame(soc_cap_all_topics_forwards[!duplicated(soc_cap_all_topics_forwards[1]),])
duplicates_diff_lim_agg <- duplicated(diff_lim_agg_all_topics_forwards[1]); length(duplicates_diff_lim_agg[duplicates_diff_lim_agg== TRUE])
diff_lim_agg_all_topics_forwards_dedup <- data.frame(diff_lim_agg_all_topics_forwards[!duplicated(diff_lim_agg_all_topics_forwards[1]),])
duplicates_emergence <- duplicated(emergence_all_topics_forwards[1]); length(duplicates_emergence[duplicates_emergence== TRUE])
emergence_all_topics_forwards_dedup <- data.frame(emergence_all_topics_forwards[!duplicated(emergence_all_topics_forwards[1]),])

## Reorder columns
soc_cap_all_topics_forwards_dedup <- soc_cap_all_topics_forwards_dedup %>% select(reference:sample_percent,doi:last_col())
diff_lim_agg_all_topics_forwards_dedup <- diff_lim_agg_all_topics_forwards_dedup %>% select(reference:sample_percent,doi:last_col())
emergence_all_topics_forwards_dedup <- emergence_all_topics_forwards_dedup %>% select(reference:sample_percent,doi:last_col())

## Compute the true depth
### soc_cap
temp_all <- soc_cap_all_topics_forwards_dedup 
last_col_1 <- ncol(temp_all)+1
last_col_2 <- ncol(temp_all)+2
for (i in seq(1,nrow(temp_all),1)) {
  references <- as.character(temp_all[i,1])
  references <- as.list(strsplit(references, '; '))
  references <- unlist(references)
  doi <- temp_all$doi
  if (nrow(temp_all[which(doi %in% references),])>0){
    temp_all[i,last_col_1] <- min(temp_all[which(doi %in% references),][,13])+1
    temp_all[i,last_col_2] <- max(temp_all[which(doi %in% references),][,13])+1
  } else {next}
  print(round(i/nrow(temp_all)*100,2))
}
names(temp_all)[length(names(temp_all))-1] <- "min_depth"
names(temp_all)[length(names(temp_all))]<-"max_depth" 
temp_all$min_depth <- ifelse(is.na(temp_all$min_depth), temp_all$depth, temp_all$min_depth)
temp_all$max_depth <- ifelse(is.na(temp_all$max_depth), temp_all$depth, temp_all$max_depth)
temp_all %>% group_by(min_depth) %>% summarise(n_val=n())
temp_all %>% group_by(max_depth) %>% summarise(n_val=n())
soc_cap_all_topics_forwards_dedup <- temp_all
### diff_lim_agg
temp_all <- diff_lim_agg_all_topics_forwards_dedup 
last_col_1 <- ncol(temp_all)+1
last_col_2 <- ncol(temp_all)+2
for (i in seq(1,nrow(temp_all),1)) {
  references <- as.character(temp_all[i,1])
  references <- as.list(strsplit(references, '; '))
  references <- unlist(references)
  doi <- temp_all$doi
  if (nrow(temp_all[which(doi %in% references),])>0){
    temp_all[i,last_col_1] <- min(temp_all[which(doi %in% references),][,13])+1
    temp_all[i,last_col_2] <- max(temp_all[which(doi %in% references),][,13])+1
  } else {next}
  print(round(i/nrow(temp_all)*100,2))
}
names(temp_all)[length(names(temp_all))-1] <- "min_depth"
names(temp_all)[length(names(temp_all))]<-"max_depth" 
temp_all$min_depth <- ifelse(is.na(temp_all$min_depth), temp_all$depth, temp_all$min_depth)
temp_all$max_depth <- ifelse(is.na(temp_all$max_depth), temp_all$depth, temp_all$max_depth)
temp_all %>% group_by(min_depth) %>% summarise(n_val=n())
temp_all %>% group_by(max_depth) %>% summarise(n_val=n())
diff_lim_agg_all_topics_forwards_dedup <- temp_all
### emergence
temp_all <- emergence_all_topics_forwards_dedup 
last_col_1 <- ncol(temp_all)+1
last_col_2 <- ncol(temp_all)+2
for (i in seq(1,nrow(temp_all),1)) {
  references <- as.character(temp_all[i,1])
  references <- as.list(strsplit(references, '; '))
  references <- unlist(references)
  doi <- temp_all$doi
  if (nrow(temp_all[which(doi %in% references),])>0){
    temp_all[i,last_col_1] <- min(temp_all[which(doi %in% references),][,13])+1
    temp_all[i,last_col_2] <- max(temp_all[which(doi %in% references),][,13])+1
  } else {next}
  print(round(i/nrow(temp_all)*100,2))
}
names(temp_all)[length(names(temp_all))-1] <- "min_depth"
names(temp_all)[length(names(temp_all))]<-"max_depth" 
temp_all$min_depth <- ifelse(is.na(temp_all$min_depth), temp_all$depth, temp_all$min_depth)
temp_all$max_depth <- ifelse(is.na(temp_all$max_depth), temp_all$depth, temp_all$max_depth)
temp_all %>% group_by(min_depth) %>% summarise(n_val=n())
temp_all %>% group_by(max_depth) %>% summarise(n_val=n())
emergence_all_topics_forwards_dedup <- temp_all

write_csv(soc_cap_all_topics_forwards_dedup,"data/soc_cap_horne_dedup_min_max.csv")
write_csv(diff_lim_agg_all_topics_forwards_dedup,"data/diff_lim_agg_horne_dedup_min_max.csv")
write_csv(emergence_all_topics_forwards_dedup,"data/emergence_horne_dedup_min_max.csv")

# Compute mean and std errors for all topics
## soc_cap
### Mean
soc_cap_topic_depth_mean <- soc_cap_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=n(),across(X0:X12, mean))
new_mean_names <- paste(colnames(soc_cap_topic_depth_mean), "_mean", sep="")
colnames(soc_cap_topic_depth_mean) <- new_mean_names
### Standard error
soc_cap_topic_depth_se <- soc_cap_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=1/1.96*sqrt(n()), across(X0:X12, sd))
soc_cap_topic_depth_se[,-(1:2)] %<>% sapply(`/`, soc_cap_topic_depth_se[,2])
new_mean_names <- paste(colnames(soc_cap_topic_depth_se), "_se", sep="")
colnames(soc_cap_topic_depth_se) <- new_mean_names
soc_cap_topic_depth_se <- subset(soc_cap_topic_depth_se,select=-c(max_depth_se,n_se))
# Make long and combine
soc_cap_topic_depth_mean_long <- gather(soc_cap_topic_depth_mean, topic, mean, X0_mean:X12_mean, factor_key=TRUE)
soc_cap_topic_depth_se_long <- gather(soc_cap_topic_depth_se, topic2, se, X0_se:X12_se, factor_key=TRUE)
soc_cap_topic_depth_mean_long <- cbind(soc_cap_topic_depth_mean_long,soc_cap_topic_depth_se_long)
# Change topic labels
soc_cap_topic_depth_mean_long$topic <- as.numeric(gsub("X","",gsub("_mean","",soc_cap_topic_depth_mean_long$topic))) + 1
# Add overall mean 
soc_cap_topic_depth_mean_long <- soc_cap_topic_depth_mean_long %>% group_by(max_depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*(1.96/sqrt(n_mean)))
## diff_lim_agg
### Mean
diff_lim_agg_topic_depth_mean <- diff_lim_agg_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=n(),across(X0:X11, mean))
new_mean_names <- paste(colnames(diff_lim_agg_topic_depth_mean), "_mean", sep="")
colnames(diff_lim_agg_topic_depth_mean) <- new_mean_names
### Standard error
diff_lim_agg_topic_depth_se <- diff_lim_agg_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=1/1.96*sqrt(n()), across(X0:X11, sd))
diff_lim_agg_topic_depth_se[,-(1:2)] %<>% sapply(`/`, diff_lim_agg_topic_depth_se[,2])
new_mean_names <- paste(colnames(diff_lim_agg_topic_depth_se), "_se", sep="")
colnames(diff_lim_agg_topic_depth_se) <- new_mean_names
diff_lim_agg_topic_depth_se <- subset(diff_lim_agg_topic_depth_se,select=-c(max_depth_se,n_se))
# Make long and combine
diff_lim_agg_topic_depth_mean_long <- gather(diff_lim_agg_topic_depth_mean, topic, mean, X0_mean:X11_mean, factor_key=TRUE)
diff_lim_agg_topic_depth_se_long <- gather(diff_lim_agg_topic_depth_se, topic2, se, X0_se:X11_se, factor_key=TRUE)
diff_lim_agg_topic_depth_mean_long <- cbind(diff_lim_agg_topic_depth_mean_long,diff_lim_agg_topic_depth_se_long)
# Change topic labels
diff_lim_agg_topic_depth_mean_long$topic <- as.numeric(gsub("X","",gsub("_mean","",diff_lim_agg_topic_depth_mean_long$topic))) + 1
# Add overall mean 
diff_lim_agg_topic_depth_mean_long <- diff_lim_agg_topic_depth_mean_long %>% group_by(max_depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*(1.96/sqrt(n_mean)))
## emergence
### Mean
emergence_topic_depth_mean <- emergence_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=n(),across(X0:X11, mean))
new_mean_names <- paste(colnames(emergence_topic_depth_mean), "_mean", sep="")
colnames(emergence_topic_depth_mean) <- new_mean_names
### Standard error
emergence_topic_depth_se <- emergence_all_topics_forwards_dedup %>%
  group_by(max_depth) %>%
  summarise(n=1/1.96*sqrt(n()), across(X0:X11, sd))
emergence_topic_depth_se[,-(1:2)] %<>% sapply(`/`, emergence_topic_depth_se[,2])
new_mean_names <- paste(colnames(emergence_topic_depth_se), "_se", sep="")
colnames(emergence_topic_depth_se) <- new_mean_names
emergence_topic_depth_se <- subset(emergence_topic_depth_se,select=-c(max_depth_se,n_se))
# Make long and combine
emergence_topic_depth_mean_long <- gather(emergence_topic_depth_mean, topic, mean, X0_mean:X11_mean, factor_key=TRUE)
emergence_topic_depth_se_long <- gather(emergence_topic_depth_se, topic2, se, X0_se:X11_se, factor_key=TRUE)
emergence_topic_depth_mean_long <- cbind(emergence_topic_depth_mean_long,emergence_topic_depth_se_long)
# Change topic labels
emergence_topic_depth_mean_long$topic <- as.numeric(gsub("X","",gsub("_mean","",emergence_topic_depth_mean_long$topic))) + 1
# Add overall mean 
emergence_topic_depth_mean_long <- emergence_topic_depth_mean_long %>% group_by(max_depth_mean)%>% mutate(mean_all=mean(mean),se_all=sd(mean)*(1.96/sqrt(n_mean)))

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
theme_soc_cap_line <- theme(plot.title = element_text(face="bold",size=24),
                              legend.position = "none",
                              legend.key.size = unit(1.5, "cm"),
                              legend.title = element_text(color="black",face="bold",size=36),
                              legend.text=element_text(color="black",size=32),
                              axis.text.x = element_text(color="black",size=36, angle=0),
                              strip.text.x = element_text(color="black",face="bold",size=36),
                              axis.title.x = element_text(color="black",size=48, angle=0),
                              axis.text.y = element_text(color="black",size=36, angle=0),
                              axis.title.y = element_text(color="black",size=48, angle=90))
# Plot topics - depth 
# Mean and standard error line plot
## soc_cap
soc_cap_topic_means <- subset(soc_cap_topic_depth_mean_long,max_depth_mean>=1 & max_depth_mean<=8) %>% 
  mutate(label=if_else(max_depth_mean==1,as.character(topic), NA_character_)) %>%
  ggplot(aes(x=max_depth_mean, y=mean, color=as.character(topic))) +
  #  geom_area(aes(x=max_depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_smooth(aes(linetype=as.character(topic)),size=2) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se,color=as.character(topic)), size=1, width=2, position=position_dodge(0.2)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(0,0.25)) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8")) + 
  labs(x="Depth", y="Topic Proportion") +
theme_classic() + theme_topic_line + geom_label_repel(aes(label = label),nudge_x = -0.3, size=12,na.rm = TRUE)
ggsave(paste0("output/soc_cap/topics/","soc_cap_topic_means.png"), plot = soc_cap_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
  ## diff_lim_agg
diff_lim_agg_topic_means <- subset(diff_lim_agg_topic_depth_mean_long,max_depth_mean>=1 & max_depth_mean<=8) %>% 
  mutate(label=if_else(max_depth_mean==1,as.character(topic), NA_character_)) %>%
  ggplot(aes(x=max_depth_mean, y=mean, color=as.character(topic))) +
  #  geom_area(aes(x=max_depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_smooth(aes(linetype=as.character(topic)),size=2) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se,color=as.character(topic)), size=1, width=2, position=position_dodge(0.2)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(0,0.25)) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8")) + 
  labs(x="Depth", y="Topic Proportion") +
theme_classic() + theme_topic_line + geom_label_repel(aes(label = label),nudge_x = -0.3, size=12,na.rm = TRUE)
ggsave(paste0("output/diff_lim_agg/topics/","diff_lim_agg_topic_means.png"), plot = diff_lim_agg_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
## emergence
emergence_topic_means <- subset(emergence_topic_depth_mean_long,max_depth_mean>=1 & max_depth_mean<=8) %>% 
  mutate(label=if_else(max_depth_mean==1,as.character(topic), NA_character_)) %>%
  ggplot(aes(x=max_depth_mean, y=mean, color=as.character(topic))) +
  #  geom_area(aes(x=max_depth_mean, y=se_all),color="red",fill="red",position = "identity") +
  geom_smooth(aes(linetype=as.character(topic)),size=2) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se,color=as.character(topic)), size=1, width=2, position=position_dodge(0.2)) + 
  coord_cartesian(xlim=c(1,8),ylim=c(0,0.25)) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8")) + 
  labs(x="Depth", y="Topic Proportion") +
theme_classic() + theme_emergence_line + geom_label_repel(aes(label = label),nudge_x = -0.3, size=12,na.rm = TRUE)
ggsave(paste0("output/emergence/topics/","emergence_topic_means.png"), plot = emergence_topic_means, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

all_topic_line_plots <- ggarrange(soc_cap_topic_means, diff_lim_agg_topic_means, emergence_topic_means, nrow = 1,labels=c("a","b","c"),
                                  font.label = list(size = 50, color = "black", face = "bold"))
ggsave(paste0("output/","all_topic_line_plots.png"), plot = all_topic_line_plots, scale = 1,
       width = 45, height = 15, dpi = 300,limitsize = TRUE)
