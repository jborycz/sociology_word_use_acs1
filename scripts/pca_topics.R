# Run PCA
soc_cap_all_topics.pca <- PCA(soc_cap_topics,ncp=10, graph = TRUE)

# Get coordinates
ind.sum <- data.frame(soc_cap_all_topics.pca$ind$coord)
png(file="output/soc_cap/scree_plot.png",width=12, height=12, units="in", res=300)
  fviz_screeplot(soc_cap_all_topics.pca, ncp=50)
dev.off()

# Includes all demos and regions
soc_cap_all_pca <- soc_cap_all
soc_cap_all_pca$Dim.1 <- ind.sum$Dim.1
soc_cap_all_pca$Dim.2 <- ind.sum$Dim.2
soc_cap_all_pca$Dim.3 <- ind.sum$Dim.3
soc_cap_all_pca$Dim.4 <- ind.sum$Dim.4
soc_cap_all_pca$Dim.5 <- ind.sum$Dim.5
soc_cap_all_pca$Dim.6 <- ind.sum$Dim.6
soc_cap_all_pca$Dim.7 <- ind.sum$Dim.7
soc_cap_all_pca$Dim.8 <- ind.sum$Dim.8
soc_cap_all_pca$Dim.9 <- ind.sum$Dim.9
soc_cap_all_pca$Dim.10 <- ind.sum$Dim.10

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
select_vars <- soc_cap_all_topics.pca$var$coord
vars <- data.frame(vars=rownames(select_vars))
select_vars <- cbind(vars,select_vars)
pca_var_plot1_2 <- ggplot(select_vars) +
  geom_point(select_vars, 
             mapping=aes(x = Dim.1, y = Dim.2,color=),size=12) +
  geom_text_repel(select_vars,mapping= aes(x = Dim.1, y = Dim.2,
                                           label=vars),color="black",size=12) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title="", x="PC1",y="PC2",color="",fill="",caption="") +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
ggsave(paste0("output/","pca_var_plot1_2.png"), plot = pca_var_plot1_2, scale = 1,
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
ggsave(paste0("output/","pca_var_plot1_3.png"), plot = pca_var_plot1_3, scale = 1,
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
ggsave(paste0("output/","pca_var_plot2_3.png"), plot = pca_var_plot2_3, scale = 1,
       width = 20,height = 20, dpi = 300,limitsize = TRUE)

## PCA individual plots
# Depth
dt_order <- unique(soc_cap_all_pca[order(soc_cap_all_pca$depth),]$depth)
soc_cap_all_pca$depth <- factor(soc_cap_all_pca$depth,levels=dt_order)
soc_cap_all_pca$dominant_topic <- colnames(soc_cap_all_topics[,16:65])[apply(soc_cap_all_topics[,16:65],1,which.max)]
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
ggsave(paste0("output/soc_cap/","pca_ind_plot1_2_topic.png"), plot = pca_ind_plot1_2, scale = 1,
       width = 15, height = 30, dpi = 300, limitsize = TRUE)




# Journal
so_order <- unique(auth_affil_stm[order(auth_affil_stm$rank_journal),]$SO)
auth_affil_stm$SO <- factor(auth_affil_stm$SO,levels=so_order)
years <- na.omit(unique(subset(auth_affil_stm,PY>=2000)$PY))
years <- years[order(years)]
for (year in years) {
  pca_ind_journal_plot1_2 <- ggplot() +
    geom_point(subset(auth_affil_stm, (dominate_topic==4 | dominate_topic==8 | dominate_topic==9 |
                                         dominate_topic==18 | dominate_topic==25 | dominate_topic==40 | dominate_topic==43 |
                                         dominate_topic==49) & rank_journal <=20 & PY==year), 
               mapping=aes(x = dim_1, y = dim_2, size=articles_journal, color = str_wrap(SO,25))) +
    facet_wrap(~dominate_topic,ncol=2) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    #  geom_text_repel(auth_affil_stm %>% group_by(dominate_topic) %>% sample_n(1),mapping=aes(x = dim_1, y = dim_2, 
    #                                                     label = paste(dominate_topic),
    #                                                     color = dominate_topic), size=4,
    #                  box.padding = unit(0.3, "lines"),
    #                  point.padding = unit(0.3, "lines")) +
    labs(title=year, x="PC1",y="PC2",color="Journal",fill="",caption="") +
    guides(color=guide_legend(ncol=1,override.aes = list(size = 12))) +
    scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
  ggexport(plotlist = list(pca_ind_journal_plot1_2), 
           filename = paste0("plots/pca_stm_journal/pca_stm_journal_",year,".png"),width=2000,height = 2000)
}






# Journal ellipse
so_order <- unique(auth_affil_stm[order(auth_affil_stm$rank_journal),]$SO)
auth_affil_stm$SO <- factor(auth_affil_stm$SO,levels=so_order)
years <- na.omit(unique(subset(auth_affil_stm,PY>=2000)$PY))
years <- years[order(years)]
for (year in years) {
  pca_ind_journal_ellipse_plot1_2 <- ggplot() +
    #  geom_point(subset(auth_affil_stm, dominate_topic==4 | dominate_topic==18), 
    #             mapping=aes(x = dim_1, y = dim_2, color = dominate_topic),size=12) +
    geom_ellipse(subset(auth_affil_stm, (dominate_topic==4 | dominate_topic==8 | dominate_topic==9 |
                                           dominate_topic==18 | dominate_topic==25 | dominate_topic==40 | dominate_topic==43 |
                                           dominate_topic==49) & rank_journal <=20 & PY==year),
                 mapping=aes(x0 = journal_dim1, y0 = journal_dim2, a = journal_dim1_sd, 
                             b = journal_dim2_sd, angle = 0,
                             fill=str_wrap(SO,25)),alpha=0.4) +
    facet_wrap(~dominate_topic,ncol=2) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    labs(title=year, x="PC1",y="PC2",color="",fill="Journal",caption="") +
    guides(fill=guide_legend(ncol=1)) +
    scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
  ggexport(plotlist = list(pca_ind_journal_ellipse_plot1_2), 
           filename = paste0("plots/pca_stm_journal_ellipse/pca_stm_journal_ellipse_",year,".png"),width=2000,height = 2000)
}

# Author ellipse
au1_order <- unique(auth_affil_stm[order(auth_affil_stm$rank_affil),]$AU1)
auth_affil_stm$AU1 <- factor(auth_affil_stm$AU1,levels=au1_order)
years <- na.omit(unique(subset(auth_affil_stm,PY>=2000)$PY))
years <- years[order(years)]
for (year in years) {
  pca_ind_auth_ellipse_plot1_2 <- ggplot() +
    geom_ellipse(subset(auth_affil_stm, (dominate_topic==4 | dominate_topic==8 | dominate_topic==9 |
                                           dominate_topic==18 | dominate_topic==25 | dominate_topic==40 | dominate_topic==43 |
                                           dominate_topic==49) & rank_auth <=20 & rank_affil <=20 & PY==year),
                 mapping=aes(x0 = auth_dim1, y0 = auth_dim2, a = auth_dim1_sd, 
                             b = auth_dim2_sd, angle = 0,
                             fill=str_wrap(AU1,25)),alpha=0.4) +
    #    facet_wrap(~dominate_topic,ncol=2) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    labs(title=year, x="PC1",y="PC2",color="",fill="Author",caption="") +
    guides(fill=guide_legend(ncol=1)) +
    coord_cartesian(xlim = c(-4,4),ylim = c(-4,4)) +
    scale_x_continuous() + scale_y_continuous() + theme_classic() + theme_point
  ggexport(plotlist = list(pca_ind_auth_ellipse_plot1_2), 
           filename = paste0("plots/pca_stm_auth_ellipse/pca_stm_auth_ellipse_",year,".png"),width=2000,height = 2000)
}



