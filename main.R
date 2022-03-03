# Libraries
library(pacman)
pacman::p_load(tidyverse,
               tidytext,
               lubridate,
               rjson,
               dplyr,
               tidyr,
               tibble,
               R.utils,
               data.table,
               stringr,
               readr,
               bibliometrix,
               tm,
               textmineR,
               purrr,
               tokenizers,
               quanteda,
               quanteda.textmodels,
               seededlda,
               FactoMineR,
               factoextra,
               ggrepel)
#pacman::p_load(bibliometrix,tm,textmineR,purrr,tokenizers,rJava,data.table,dplyr,tidyr,
#               tidyverse,tidytext,ggpubr,qdap,ggforce,word2vec,cowplot,ggpmisc,ggrepel,
#               FactoMineR,factoextra,word2vec)

# Load data
soc_cap_all <- read.csv("data/soc_cap_all.csv")
diff_lim_agg_all <- read.csv("data/diff_lim_agg_all.csv")
emergence_all <- read.csv("data/emergence_all.csv")

# Select sample information
# soc_cap_all <- subset(soc_cap_all,sample_percent=="100" | sample_percent=="20")
# diff_lim_agg_all <- subset(diff_lim_agg_all,sample_percent=="100" | sample_percent=="10")
# emergence_all <- subset(emergence_all,sample_percent=="100" | sample_percent=="5")

# Clean the tiles column of the dataframes
source("scripts/clean_titles.R")

# Create document frequency matrix and run a topic model
source("scripts/topic_model.R")
