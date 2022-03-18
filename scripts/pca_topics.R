# Load in Ben's topic data
soc_cap_topics_forwards2 <- data.frame(subset(soc_cap_topics_forwards,select=-c(doi)))
diff_lim_agg_topics_forwards2 <- data.frame(subset(diff_lim_agg_topics_forwards,select=-c(doi)))
emergence_topics_forwards2 <- data.frame(subset(emergence_topics_forwards,select=-c(doi)))

# Run PCA
soc_cap_all_topics.pca <- PCA(soc_cap_topics_forwards2,ncp=20, graph = TRUE)
diff_lim_agg_all_topics.pca <- PCA(diff_lim_agg_topics_forwards2,ncp=20, graph = TRUE)
emergence_all_topics.pca <- PCA(emergence_topics_forwards2,ncp=20, graph = TRUE)

# Get coordinates
soc_cap_ind.sum <- data.frame(soc_cap_all_topics.pca$ind$coord)
diff_lim_agg_ind.sum <- data.frame(diff_lim_agg_all_topics.pca$ind$coord)
emergence_ind.sum <- data.frame(emergence_all_topics.pca$ind$coord)

# Export scree
png(file="output/soc_cap/pca/scree_plot.png",width=12, height=12, units="in", res=300)
  fviz_screeplot(soc_cap_all_topics.pca, ncp=12)
dev.off()
png(file="output/diff_lim_agg/pca/scree_plot.png",width=12, height=12, units="in", res=300)
fviz_screeplot(diff_lim_agg_all_topics.pca, ncp=11)
dev.off()
png(file="output/emergence/pca/scree_plot.png",width=12, height=12, units="in", res=300)
fviz_screeplot(emergence_all_topics.pca, ncp=11)
dev.off()

# Includes all demos and regions
## soc_cap
soc_cap_all_pca <- soc_cap_all_topics_forwards_dedup
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
diff_lim_agg_all_pca <- diff_lim_agg_all_topics_forwards_dedup
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
emergence_all_pca <- emergence_all_topics_forwards_dedup
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
### soc_cap
paper_name <- "soc_cap"
select_vars <- soc_cap_all_topics.pca$var$coord
### Create plots
vars <- data.frame(vars=rownames(select_vars))
select_vars <- cbind(vars,select_vars)
pca_var_plot1_2 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.2),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.2,
                                           label=as.numeric(gsub("X","",vars))+1),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/soc_cap_pca_var_plot1_2.png"), plot = pca_var_plot1_2, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
### soc_cap
paper_name <- "diff_lim_agg"
select_vars <- diff_lim_agg_all_topics.pca$var$coord
### Create plots
vars <- data.frame(vars=rownames(select_vars))
select_vars <- cbind(vars,select_vars)
pca_var_plot1_2 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.2),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.2,
                                           label=as.numeric(gsub("X","",vars))+1),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/diff_lim_agg_pca_var_plot1_2.png"), plot = pca_var_plot1_2, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)
### emergence
paper_name <- "emergence"
select_vars <- emergence_all_topics.pca$var$coord
### Create plots
vars <- data.frame(vars=rownames(select_vars))
select_vars <- cbind(vars,select_vars)
pca_var_plot1_2 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.2),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.2,
                                           label=as.numeric(gsub("X","",vars))+1),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/",paper_name,"/pca/emergence_pca_var_plot1_2.png"), plot = pca_var_plot1_2, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

## PCA individual plots
# Depth
## soc_cap
dt_order <- unique(soc_cap_all_pca[order(soc_cap_all_pca$max_depth),]$max_depth)
soc_cap_all_pca$max_depth <- factor(soc_cap_all_pca$max_depth,levels=dt_order)
soc_cap_all_pca$dominant_topic <- colnames(soc_cap_all_pca[,16:28])[apply(soc_cap_all_pca[,16:28],1,which.max)]
soc_cap_all_pca$dominant_topic <- as.numeric(gsub("X","",soc_cap_all_pca$dominant_topic))+1
dt_order2 <- unique(soc_cap_all_pca[order(as.numeric(soc_cap_all_pca$dominant_topic)),]$dominant_topic)
soc_cap_all_pca$dominant_topic <- factor(soc_cap_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- subset(soc_cap_all_pca,as.numeric(max_depth)>=1 & as.numeric(max_depth)<=7) %>% ggplot() +
  geom_point(mapping=aes(x = Dim.1,y = Dim.2, color=dominant_topic),size=4) +
  facet_wrap(~max_depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=1)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/soc_cap/pca/","soc_cap_pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)
# diff_lim_agg
dt_order <- unique(diff_lim_agg_all_pca[order(diff_lim_agg_all_pca$depth),]$depth)
diff_lim_agg_all_pca$max_depth <- factor(diff_lim_agg_all_pca$depth,levels=dt_order)
diff_lim_agg_all_pca$dominant_topic <- colnames(diff_lim_agg_all_pca[,16:27])[apply(diff_lim_agg_all_pca[,16:27],1,which.max)]
diff_lim_agg_all_pca$dominant_topic <- as.numeric(gsub("X","",diff_lim_agg_all_pca$dominant_topic))+1
dt_order2 <- unique(diff_lim_agg_all_pca[order(as.numeric(diff_lim_agg_all_pca$dominant_topic)),]$dominant_topic)
diff_lim_agg_all_pca$dominant_topic <- factor(diff_lim_agg_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- subset(diff_lim_agg_all_pca,as.numeric(max_depth)>=0 & as.numeric(max_depth)<=6) %>% ggplot() +
  geom_point(mapping=aes(x = Dim.1,y = Dim.2, color=dominant_topic),size=4) +
  facet_wrap(~depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=1)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/diff_lim_agg/pca/","diff_lim_agg_pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)
# emergence
dt_order <- unique(emergence_all_pca[order(emergence_all_pca$max_depth),]$max_depth)
emergence_all_pca$max_depth <- factor(emergence_all_pca$max_depth,levels=dt_order)
emergence_all_pca$dominant_topic <- colnames(emergence_all_pca[,16:27])[apply(emergence_all_pca[,16:27],1,which.max)]
emergence_all_pca$dominant_topic <- as.numeric(gsub("X","",emergence_all_pca$dominant_topic))+1
dt_order2 <- unique(emergence_all_pca[order(as.numeric(emergence_all_pca$dominant_topic)),]$dominant_topic)
emergence_all_pca$dominant_topic <- factor(emergence_all_pca$dominant_topic,levels=dt_order2)
pca_ind_plot1_2 <- subset(emergence_all_pca,as.numeric(max_depth)>=0 & as.numeric(max_depth)<=7) %>% ggplot() +
  geom_point(mapping=aes(x = Dim.1,y = Dim.2, color=dominant_topic),size=4) +
  facet_wrap(~max_depth,ncol=2) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="Topic",fill="",caption="") +
  guides(color=guide_legend(ncol=1)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/emergence/pca/","emergence_pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)

