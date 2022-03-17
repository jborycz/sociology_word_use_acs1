#################
### Backwards ###
#################
soc_cap_topics_backwards <- read_csv("data/metadata_match/doc_to_topics_soc_cap_backwards_5.csv")
diff_lim_agg_topics_backwards <- read_csv("data/metadata_match/doc_to_topics_diff_lim_agg_backwards_9.csv")
emergence_topics_backwards <- read_csv("data/metadata_match/doc_to_topics_emergence_backwards_10.csv")
names(soc_cap_topics_backwards)[names(soc_cap_topics_backwards) == "id"] <- "doi"
names(diff_lim_agg_topics_backwards)[names(diff_lim_agg_topics_backwards) == "id"] <- "doi"
names(emergence_topics_backwards)[names(emergence_topics_backwards) == "id"] <- "doi"

## Join with metadata
soc_cap_all_topics_backwards <- left_join(soc_cap_topics_backwards,soc_cap_all,by="doi")
diff_lim_agg_all_topics_backwards <- left_join(diff_lim_agg_topics_backwards,diff_lim_agg_all,by="doi")
emergence_all_topics_backwards <- left_join(emergence_topics_backwards,emergence_all,by="doi")

## Filter out repeats
duplicates_soc_cap <- duplicated(soc_cap_all_topics_backwards[1]); length(duplicates_soc_cap[duplicates_soc_cap== TRUE])
soc_cap_all_topics_backwards_dedup <- data.frame(soc_cap_all_topics_backwards[!duplicated(soc_cap_all_topics_backwards[1]),])
duplicates_diff_lim_agg <- duplicated(diff_lim_agg_all_topics_backwards[1]); length(duplicates_diff_lim_agg[duplicates_diff_lim_agg== TRUE])
diff_lim_agg_all_topics_backwards_dedup <- data.frame(diff_lim_agg_all_topics_backwards[!duplicated(diff_lim_agg_all_topics_backwards[1]),])
duplicates_emergence <- duplicated(emergence_all_topics_backwards[1]); length(duplicates_emergence[duplicates_emergence== TRUE])
emergence_all_topics_backwards_dedup <- data.frame(emergence_all_topics_backwards[!duplicated(emergence_all_topics_backwards[1]),])

# Get n
soc_cap_topic_depth_mean2 <- soc_cap_all_topics_backwards_dedup %>% group_by(max_depth) %>% summarise(n=n())
diff_lim_agg_topic_depth_mean2 <- diff_lim_agg_all_topics_backwards_dedup %>% group_by(max_depth) %>% summarise(n=n())
emergence_topic_depth_mean2 <- emergence_all_topics_backwards_dedup %>% group_by(max_depth) %>% summarise(n=n())

################
### Forwards ###
################
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

# Get n
soc_cap_topic_depth_mean3 <- soc_cap_all_topics_forwards_dedup %>% group_by(depth) %>% summarise(n=n())
diff_lim_agg_topic_depth_mean3 <- diff_lim_agg_all_topics_forwards_dedup %>% group_by(depth) %>% summarise(n=n())
emergence_topic_depth_mean3 <- emergence_all_topics_forwards_dedup %>% group_by(depth) %>% summarise(n=n())

################
### Breadth  ###
################
soc_cap_topics_breadth <- read_csv("data/metadata_match/doc_to_topics_soc_cap_breadth_7.csv")
diff_lim_agg_topics_breadth <- read_csv("data/metadata_match/doc_to_topics_diff_lim_agg_breadth_14.csv")
emergence_topics_breadth <- read_csv("data/metadata_match/doc_to_topics_emergence_breadth_17.csv")
names(soc_cap_topics_breadth)[names(soc_cap_topics_breadth) == "id"] <- "doi"
names(diff_lim_agg_topics_breadth)[names(diff_lim_agg_topics_breadth) == "id"] <- "doi"
names(emergence_topics_breadth)[names(emergence_topics_breadth) == "id"] <- "doi"

## Join with metadata
soc_cap_all_topics_breadth <- left_join(soc_cap_topics_breadth,soc_cap_all,by="doi")
diff_lim_agg_all_topics_breadth <- left_join(diff_lim_agg_topics_breadth,diff_lim_agg_all,by="doi")
emergence_all_topics_breadth <- left_join(emergence_topics_breadth,emergence_all,by="doi")

## Filter out repeats
duplicates_soc_cap <- duplicated(soc_cap_all_topics_breadth[1]); length(duplicates_soc_cap[duplicates_soc_cap== TRUE])
soc_cap_all_topics_breadth_dedup <- data.frame(soc_cap_all_topics_breadth[!duplicated(soc_cap_all_topics_breadth[1]),])
duplicates_diff_lim_agg <- duplicated(diff_lim_agg_all_topics_breadth[1]); length(duplicates_diff_lim_agg[duplicates_diff_lim_agg== TRUE])
diff_lim_agg_all_topics_breadth_dedup <- data.frame(diff_lim_agg_all_topics_breadth[!duplicated(diff_lim_agg_all_topics_breadth[1]),])
duplicates_emergence <- duplicated(emergence_all_topics_breadth[1]); length(duplicates_emergence[duplicates_emergence== TRUE])
emergence_all_topics_breadth_dedup <- data.frame(emergence_all_topics_breadth[!duplicated(emergence_all_topics_breadth[1]),])

# Get n
soc_cap_topic_depth_mean4 <- soc_cap_all_topics_breadth_dedup %>% group_by(max_depth) %>% summarise(n=n())
diff_lim_agg_topic_depth_mean4 <- diff_lim_agg_all_topics_breadth_dedup %>% group_by(max_depth) %>% summarise(n=n())
emergence_topic_depth_mean4 <- emergence_all_topics_breadth_dedup %>% group_by(max_depth) %>% summarise(n=n())

all_n <- plyr::join_all(list(diff_lim_agg_topic_depth_mean4,
                             soc_cap_topic_depth_mean4,
                             emergence_topic_depth_mean4,
                             diff_lim_agg_topic_depth_mean2,
                             soc_cap_topic_depth_mean2,
                             emergence_topic_depth_mean2,
                             diff_lim_agg_topic_depth_mean3,
                             soc_cap_topic_depth_mean3,
                             emergence_topic_depth_mean3), by="depth", type="left", match = "all")
colnames(all_n) <- c("depth","diff_lim_agg_breadth","soc_cap_breadth","emergence_breadth",
                     "diff_lim_agg_backwards","soc_cap_backwards","emergence_backwards",
                     "diff_lim_agg_forwards","soc_cap_forwards","emergence_forwards")

all_n <- plyr::join_all(list(diff_lim_agg_topic_depth_mean3,
                             soc_cap_topic_depth_mean3,
                             emergence_topic_depth_mean3), by="depth", type="left", match = "all")
colnames(all_n) <- c("depth","diff_lim_agg_forwards","soc_cap_forwards","emergence_forwards")

