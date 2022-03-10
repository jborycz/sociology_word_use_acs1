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

