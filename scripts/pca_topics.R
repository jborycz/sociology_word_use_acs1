# Run PCA
soc_cap_all_topics.pca <- PCA(soc_cap_topics,ncp=20, graph = TRUE)
diff_lim_agg_all_topics.pca <- PCA(diff_lim_agg_topics,ncp=20, graph = TRUE)
emergence_all_topics.pca <- PCA(emergence_topics,ncp=20, graph = TRUE)

# Get coordinates
soc_cap_ind.sum <- data.frame(soc_cap_all_topics.pca$ind$coord)
diff_lim_agg_ind.sum <- data.frame(diff_lim_agg_all_topics.pca$ind$coord)
emergence_ind.sum <- data.frame(emergence_all_topics.pca$ind$coord)

# Export scree
png(file="output/soc_cap/pca/scree_plot.png",width=12, height=12, units="in", res=300)
  fviz_screeplot(soc_cap_all_topics.pca, ncp=50)
dev.off()
png(file="output/diff_lim_agg/pca/scree_plot.png",width=12, height=12, units="in", res=300)
fviz_screeplot(diff_lim_agg_all_topics.pca, ncp=50)
dev.off()
png(file="output/emergence/pca/scree_plot.png",width=12, height=12, units="in", res=300)
fviz_screeplot(emergence_all_topics.pca, ncp=50)
dev.off()

# Includes all demos and regions
## soc_cap
soc_cap_all_pca <- soc_cap_all
soc_cap_all_pca$Dim.1 <- soc_cap_ind.sum$Dim.1
soc_cap_all_pca$Dim.2 <- soc_cap_ind.sum$Dim.2
soc_cap_all_pca$Dim.3 <- soc_cap_ind.sum$Dim.3
soc_cap_all_pca$Dim.4 <- soc_cap_ind.sum$Dim.4
soc_cap_all_pca$Dim.5 <- soc_cap_ind.sum$Dim.5
soc_cap_all_pca$Dim.6 <- soc_cap_ind.sum$Dim.6
soc_cap_all_pca$Dim.7 <- soc_cap_ind.sum$Dim.7
soc_cap_all_pca$Dim.8 <- soc_cap_ind.sum$Dim.8
soc_cap_all_pca$Dim.9 <- soc_cap_ind.sum$Dim.9
soc_cap_all_pca$Dim.10 <- soc_cap_ind.sum$Dim.10
## diff_lim_agg
diff_lim_agg_all_pca <- diff_lim_agg_all
diff_lim_agg_all_pca$Dim.1 <- diff_lim_agg_ind.sum$Dim.1
diff_lim_agg_all_pca$Dim.2 <- diff_lim_agg_ind.sum$Dim.2
diff_lim_agg_all_pca$Dim.3 <- diff_lim_agg_ind.sum$Dim.3
diff_lim_agg_all_pca$Dim.4 <- diff_lim_agg_ind.sum$Dim.4
diff_lim_agg_all_pca$Dim.5 <- diff_lim_agg_ind.sum$Dim.5
diff_lim_agg_all_pca$Dim.6 <- diff_lim_agg_ind.sum$Dim.6
diff_lim_agg_all_pca$Dim.7 <- diff_lim_agg_ind.sum$Dim.7
diff_lim_agg_all_pca$Dim.8 <- diff_lim_agg_ind.sum$Dim.8
diff_lim_agg_all_pca$Dim.9 <- diff_lim_agg_ind.sum$Dim.9
diff_lim_agg_all_pca$Dim.10 <- diff_lim_agg_ind.sum$Dim.10
## emergence
emergence_all_pca <- emergence_all
emergence_all_pca$Dim.1 <- emergence_ind.sum$Dim.1
emergence_all_pca$Dim.2 <- emergence_ind.sum$Dim.2
emergence_all_pca$Dim.3 <- emergence_ind.sum$Dim.3
emergence_all_pca$Dim.4 <- emergence_ind.sum$Dim.4
emergence_all_pca$Dim.5 <- emergence_ind.sum$Dim.5
emergence_all_pca$Dim.6 <- emergence_ind.sum$Dim.6
emergence_all_pca$Dim.7 <- emergence_ind.sum$Dim.7
emergence_all_pca$Dim.8 <- emergence_ind.sum$Dim.8
emergence_all_pca$Dim.9 <- emergence_ind.sum$Dim.9
emergence_all_pca$Dim.10 <- emergence_ind.sum$Dim.10

### Plots
## Themes
theme_point <- theme(plot.title = element_text(face="bold",size=40),
                     legend.position = "right",
                     legend.key.size = unit(1.5, "cm"),
                     legend.title = element_text(color="black",face="bold",size=36),
                     legend.text=element_text(color="black",size=32),
                     axis.text.x = element_text(color="black",size=36, angle=0),
                     strip.text.x = element_text(color="black",face="bold",size=36),
                     axis.title.x = element_text(color="black",size=48, angle=0),
                     axis.text.y = element_text(color="black",size=36, angle=0),
                     axis.title.y = element_text(color="black",size=48, angle=90))

## PCA variable plots
### Choose paper
paper_name <- "emergence"
select_vars <- emergence_all_topics.pca$var$coord
### Create plots
vars <- data.frame(vars=rownames(select_vars))
select_vars <- cbind(vars,select_vars)
pca_var_plot1_2 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.2),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.2,
                                           label=vars),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/pca_var_plot1_2.png"), plot = pca_var_plot1_2, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

pca_var_plot1_3 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.3),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.3,
                                           label=vars),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC3",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/pca_var_plot1_3.png"), plot = pca_var_plot1_3, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

pca_var_plot2_3 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.2, y = Dim.3),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.2, y = Dim.3,
                                           label=vars),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC3",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/pca_var_plot2_3.png"), plot = pca_var_plot2_3, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

## PCA individual plots
# Depth
## soc_cap
dt_order <- unique(soc_cap_all_pca[order(soc_cap_all_pca$depth),]$depth)
soc_cap_all_pca$depth <- factor(soc_cap_all_pca$depth,levels=dt_order)
soc_cap_all_pca$dominant_topic <- colnames(soc_cap_all_topics[,17:66])[apply(soc_cap_all_topics[,17:66],1,which.max)]
soc_cap_all_pca$dominant_topic <- gsub("topic","",soc_cap_all_pca$dominant_topic)
dt_order2 <- unique(soc_cap_all_pca[order(as.numeric(soc_cap_all_pca$dominant_topic)),]$dominant_topic)
soc_cap_all_pca$dominant_topic <- factor(soc_cap_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- ggplot() +
  geom_point(soc_cap_all_pca, mapping=aes(x = Dim.1,y = Dim.2, color=dominant_topic),size=4) +
  facet_wrap(~depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
#  geom_text_repel(soc_cap_all_pca %>% group_by(dominant_topic) %>% sample_n(1),
#                  mapping=aes(x = Dim.1,y = Dim.2,label = paste(dominant_topic),
#                              color = dominant_topic), size=4,
#                  point.padding = unit(0.3, "lines")) +
  labs(title="", x="PC1",y="PC2",color="Dominant Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/soc_cap/pca/","pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)
# diff_lim_agg
dt_order <- unique(diff_lim_agg_all_pca[order(diff_lim_agg_all_pca$depth),]$depth)
diff_lim_agg_all_pca$depth <- factor(diff_lim_agg_all_pca$depth,levels=dt_order)
diff_lim_agg_all_pca$dominant_topic <- colnames(diff_lim_agg_all_topics[,16:65])[apply(diff_lim_agg_all_topics[,16:65],1,which.max)]
diff_lim_agg_all_pca$dominant_topic <- gsub("topic","",diff_lim_agg_all_pca$dominant_topic)
dt_order2 <- unique(diff_lim_agg_all_pca[order(as.numeric(diff_lim_agg_all_pca$dominant_topic)),]$dominant_topic)
diff_lim_agg_all_pca$dominant_topic <- factor(diff_lim_agg_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- ggplot() +
  geom_point(diff_lim_agg_all_pca, mapping=aes(x = Dim.1,y = Dim.2),color="grey",size=3) +
#  geom_point(subset(diff_lim_agg_all_pca,dominant_topic=="3"|dominant_topic=="26"), mapping=aes(x = Dim.1,y = Dim.2,color=dominant_topic),size=3) +
  facet_wrap(~depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  #  geom_text_repel(diff_lim_agg_all_pca %>% group_by(dominant_topic) %>% sample_n(1),
  #                  mapping=aes(x = Dim.1,y = Dim.2,label = paste(dominant_topic),
  #                              color = dominant_topic), size=4,
  #                  point.padding = unit(0.3, "lines")) +
  labs(title="", x="PC1",y="PC2",color="Dominant Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/diff_lim_agg/pca/","pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)
# emergence
dt_order <- unique(emergence_all_pca[order(emergence_all_pca$depth),]$depth)
emergence_all_pca$depth <- factor(emergence_all_pca$depth,levels=dt_order)
emergence_all_pca$dominant_topic <- colnames(emergence_all_topics[,16:65])[apply(emergence_all_topics[,15:65],1,which.max)]
emergence_all_pca$dominant_topic <- gsub("topic","",emergence_all_pca$dominant_topic)
dt_order2 <- unique(emergence_all_pca[order(as.numeric(emergence_all_pca$dominant_topic)),]$dominant_topic)
emergence_all_pca$dominant_topic <- factor(emergence_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- ggplot() +
  geom_point(emergence_all_pca, mapping=aes(x = Dim.1,y = Dim.2),color="grey",size=3) +
  geom_point(subset(diff_lim_agg_all_pca,dominant_topic=="10"|dominant_topic=="39"), mapping=aes(x = Dim.1,y = Dim.2,color=dominant_topic),size=3) +
  facet_wrap(~depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  #  geom_text_repel(emergence_all_pca %>% group_by(dominant_topic) %>% sample_n(1),
  #                  mapping=aes(x = Dim.1,y = Dim.2,label = paste(dominant_topic),
  #                              color = dominant_topic), size=4,
  #                  point.padding = unit(0.3, "lines")) +
  labs(title="", x="PC1",y="PC2",color="Dominant Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/emergence/pca/","pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)

