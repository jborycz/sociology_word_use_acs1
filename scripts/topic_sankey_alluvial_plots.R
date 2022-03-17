# Add dominant topic to topic dataframe
## soc_cap
soc_cap_dt_order <- unique(soc_cap_all_topics[order(soc_cap_all_topics$depth),]$depth)
soc_cap_all_topics$depth <- factor(soc_cap_all_topics$depth,levels=soc_cap_dt_order)
soc_cap_all_topics$dominant_topic <- colnames(soc_cap_all_topics[,16:65])[apply(soc_cap_all_topics[,16:65],1,which.max)]
soc_cap_all_topics$dominant_topic <- gsub("topic","",soc_cap_all_topics$dominant_topic)
soc_cap_dt_order2 <- unique(soc_cap_all_topics[order(as.numeric(soc_cap_all_topics$dominant_topic)),]$dominant_topic)
soc_cap_all_topics$dominant_topic <- factor(soc_cap_all_topics$dominant_topic,levels=soc_cap_dt_order2)
## diff_lim_agg
diff_lim_agg_dt_order <- unique(diff_lim_agg_all_topics[order(diff_lim_agg_all_topics$depth),]$depth)
diff_lim_agg_all_topics$depth <- factor(diff_lim_agg_all_topics$depth,levels=diff_lim_agg_dt_order)
diff_lim_agg_all_topics$dominant_topic <- colnames(diff_lim_agg_all_topics[,16:65])[apply(diff_lim_agg_all_topics[,16:65],1,which.max)]
diff_lim_agg_all_topics$dominant_topic <- gsub("topic","",diff_lim_agg_all_topics$dominant_topic)
diff_lim_agg_dt_order2 <- unique(diff_lim_agg_all_topics[order(as.numeric(diff_lim_agg_all_topics$dominant_topic)),]$dominant_topic)
diff_lim_agg_all_topics$dominant_topic <- factor(diff_lim_agg_all_topics$dominant_topic,levels=diff_lim_agg_dt_order2)
## emergence
emergence_dt_order <- unique(emergence_all_topics[order(emergence_all_topics$depth),]$depth)
emergence_all_topics$depth <- factor(emergence_all_topics$depth,levels=emergence_dt_order)
emergence_all_topics$dominant_topic <- colnames(emergence_all_topics[,16:65])[apply(emergence_all_topics[,16:65],1,which.max)]
emergence_all_topics$dominant_topic <- gsub("topic","",emergence_all_topics$dominant_topic)
emergence_dt_order2 <- unique(emergence_all_topics[order(as.numeric(emergence_all_topics$dominant_topic)),]$dominant_topic)
emergence_all_topics$dominant_topic <- factor(emergence_all_topics$dominant_topic,levels=emergence_dt_order2)

# Subset by interesting topics
## soc_cap
#soc_cap_all_topics_int <- subset(soc_cap_all_topics,dominant_topic=="3" | dominant_topic=="26")
## diff_lim_agg
diff_lim_agg_all_topics_int <- subset(diff_lim_agg_all_topics,dominant_topic=="3" | dominant_topic=="26")
diff_lim_agg_topics_int_long <- gather(diff_lim_agg_all_topics_int, topic, value, topic1:topic50)
diff_lim_agg_topics_int_long$topic <- gsub("topic","",diff_lim_agg_topics_int_long$topic)

## emergence
emergence_all_topics_int <- subset(emergence_all_topics,dominant_topic=="10" | dominant_topic=="39")
emergence_topics_int_long <- gather(emergence_all_topics_int, topic, value, topic1:topic50)
emergence_topics_int_long$topic <- gsub("topic","",emergence_topics_int_long$topic)

# Alluvial plots
## Theme
theme_alluvial <- theme(plot.title = element_text(face="bold",size=40),
                     legend.position = "right",
                     legend.key.size = unit(1.5, "cm"),
                     legend.title = element_text(color="black",face="bold",size=36),
                     legend.text=element_text(color="black",size=32),
                     axis.text.x = element_text(color="black",size=36, angle=0),
                     strip.text.x = element_text(color="black",face="bold",size=36),
                     axis.title.x = element_text(color="black",size=48, angle=0),
                     axis.text.y = element_text(color="black",size=36, angle=0),
                     axis.title.y = element_text(color="black",size=48, angle=90))

diff_lim_agg_topics_int_long$topic <- as.factor(diff_lim_agg_topics_int_long$topic)
diff_lim_agg_alluvial <- ggplot(diff_lim_agg_topics_int_long,
       aes(x = depth, stratum = topic, alluvium = dominant_topic,
           fill = topic, label = topic)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("")
ggsave(paste0("output/topics/diff_lim_agg_alluvial.png"), plot = diff_lim_agg_alluvial, scale = 1,
       width = 30,height = 20, dpi = 300,limitsize = TRUE)

