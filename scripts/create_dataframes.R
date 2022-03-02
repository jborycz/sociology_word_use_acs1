# Create citation dataframes
## "DIFFUSION-LIMITED AGGREGATION, A KINETIC CRITICAL PHENOMENON"
### Complete set of citations (1-99999 citation limits)
diff_lim_agg_1_49_0 <- read.csv("data/acs_sociology_abstracts/cit_1_49/diff_lim_agg/diff_lim_agg_0_complete.csv")
diff_lim_agg_1_49_1 <- read.csv("data/acs_sociology_abstracts/cit_1_49/diff_lim_agg/diff_lim_agg_1_complete.csv")
diff_lim_agg_1_49_2 <- read.csv("data/acs_sociology_abstracts/cit_1_49/diff_lim_agg/diff_lim_agg_2_complete.csv")
diff_lim_agg_50_99_1 <- read.csv("data/acs_sociology_abstracts/cit_50_99/diff_lim_agg/diff_lim_agg_1_complete.csv")
diff_lim_agg_50_99_2 <- read.csv("data/acs_sociology_abstracts/cit_50_99/diff_lim_agg/diff_lim_agg_2_complete.csv")
diff_lim_agg_100_199_1 <- read.csv("data/acs_sociology_abstracts/cit_100_199/diff_lim_agg/diff_lim_agg_1_complete.csv")
diff_lim_agg_100_199_2 <- read.csv("data/acs_sociology_abstracts/cit_100_199/diff_lim_agg/diff_lim_agg_2_complete.csv")
diff_lim_agg_200_99999_1 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/diff_lim_agg/diff_lim_agg_1_complete.csv")
diff_lim_agg_200_99999_2 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/diff_lim_agg/diff_lim_agg_2_complete.csv")
diff_lim_agg_complete <- rbind(diff_lim_agg_1_49_0, diff_lim_agg_1_49_1, diff_lim_agg_1_49_2,
                           diff_lim_agg_50_99_1,diff_lim_agg_50_99_2,
                           diff_lim_agg_100_199_1,diff_lim_agg_100_199_2,
                           diff_lim_agg_200_99999_1,diff_lim_agg_200_99999_2)
diff_lim_agg_complete$sample_percent <- "100"

### Samples by percentage (5-4000 citation liimits)
diff_lim_agg_sample_5_3 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_3_complete.csv")
diff_lim_agg_sample_5_4 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_4_complete.csv")
diff_lim_agg_sample_5_5 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_5_complete.csv")
diff_lim_agg_sample_5_6 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_6_complete.csv")
diff_lim_agg_sample_5_7 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_7_complete.csv")
diff_lim_agg_sample_5_8 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_8_complete.csv")
diff_lim_agg_sample_5_9 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_9_complete.csv")
diff_lim_agg_sample_5_10 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_10_complete.csv")
diff_lim_agg_sample_5_11 <- read.csv("data/acs_sociology_abstracts/sample_5/diff_lim_agg/diff_lim_agg_11_complete.csv")
diff_lim_agg_sample_10_3 <- read.csv("data/acs_sociology_abstracts/sample_10/diff_lim_agg/diff_lim_agg_3_complete.csv")
diff_lim_agg_sample_10_4 <- read.csv("data/acs_sociology_abstracts/sample_10/diff_lim_agg/diff_lim_agg_4_complete.csv")
diff_lim_agg_sample_10_5 <- read.csv("data/acs_sociology_abstracts/sample_10/diff_lim_agg/diff_lim_agg_5_complete.csv")
diff_lim_agg_sample_10_6 <- read.csv("data/acs_sociology_abstracts/sample_10/diff_lim_agg/diff_lim_agg_6_complete.csv")
diff_lim_agg_sample_10_7 <- read.csv("data/acs_sociology_abstracts/sample_10/diff_lim_agg/diff_lim_agg_7_complete.csv")
diff_lim_agg_sample_15_3 <- read.csv("data/acs_sociology_abstracts/sample_15/diff_lim_agg/diff_lim_agg_3_complete.csv")
diff_lim_agg_sample_20_3 <- read.csv("data/acs_sociology_abstracts/sample_20/diff_lim_agg/diff_lim_agg_3_complete.csv")
diff_lim_agg_sample_5_3$sample_percent <- "5"
diff_lim_agg_sample_5_4$sample_percent <- "5"
diff_lim_agg_sample_5_5$sample_percent <- "5"
diff_lim_agg_sample_5_6$sample_percent <- "5"
diff_lim_agg_sample_5_7$sample_percent <- "5"
diff_lim_agg_sample_5_8$sample_percent <- "5"
diff_lim_agg_sample_5_9$sample_percent <- "5"
diff_lim_agg_sample_5_10$sample_percent <- "5"
diff_lim_agg_sample_5_11$sample_percent <- "5"
diff_lim_agg_sample_10_3$sample_percent <- "10"
diff_lim_agg_sample_10_4$sample_percent <- "10"
diff_lim_agg_sample_10_5$sample_percent <- "10"
diff_lim_agg_sample_10_6$sample_percent <- "10"
diff_lim_agg_sample_10_7$sample_percent <- "10"
diff_lim_agg_sample_15_3$sample_percent <- "15"
diff_lim_agg_sample_20_3$sample_percent <- "20"
diff_lim_agg_sample <- rbind(diff_lim_agg_sample_5_3, diff_lim_agg_sample_5_4, diff_lim_agg_sample_5_5,
                             diff_lim_agg_sample_5_6, diff_lim_agg_sample_5_7, diff_lim_agg_sample_5_8,
                             diff_lim_agg_sample_5_9,diff_lim_agg_sample_5_10,diff_lim_agg_sample_5_11,
                             diff_lim_agg_sample_10_3,diff_lim_agg_sample_10_4,diff_lim_agg_sample_10_5,
                             diff_lim_agg_sample_10_6,diff_lim_agg_sample_10_7,
                             diff_lim_agg_sample_15_3,
                             diff_lim_agg_sample_20_3)

### Combine complete citations with sample citations and save to csv
diff_lim_agg_all <- rbind(diff_lim_agg_complete,diff_lim_agg_sample)
names(diff_lim_agg_all)[names(diff_lim_agg_all) == "cit_level"] <- "depth"
### Remove duplicates
duplicates_diff_lim_agg <- duplicated(diff_lim_agg_all[c(12,14,15)])
length(duplicates_diff_lim_agg[duplicates_diff_lim_agg== TRUE])
diff_lim_agg_all <- diff_lim_agg_all[!duplicated(diff_lim_agg_all[c(12,14,15)]),]
write_excel_csv(diff_lim_agg_all,"data/diff_lim_agg_all.csv")

## "Social capital, intellectual capital, and the organizational advantage"
### Complete set of citations (1-99999 citation limits)
soc_cap_1_49_0 <- read.csv("data/acs_sociology_abstracts/cit_1_49/soc_cap/soc_cap_0_complete.csv")
soc_cap_1_49_1 <- read.csv("data/acs_sociology_abstracts/cit_1_49/soc_cap/soc_cap_1_complete.csv")
soc_cap_1_49_2 <- read.csv("data/acs_sociology_abstracts/cit_1_49/soc_cap/soc_cap_2_complete.csv")
soc_cap_1_49_3 <- read.csv("data/acs_sociology_abstracts/cit_1_49/soc_cap/soc_cap_3_complete.csv")
soc_cap_50_99_1 <- read.csv("data/acs_sociology_abstracts/cit_50_99/soc_cap/soc_cap_1_complete.csv")
soc_cap_50_99_2 <- read.csv("data/acs_sociology_abstracts/cit_50_99/soc_cap/soc_cap_2_complete.csv")
soc_cap_50_99_3 <- read.csv("data/acs_sociology_abstracts/cit_50_99/soc_cap/soc_cap_3_complete.csv")
soc_cap_100_149_1 <- read.csv("data/acs_sociology_abstracts/cit_100_149/soc_cap/soc_cap_1_complete.csv")
soc_cap_100_149_2 <- read.csv("data/acs_sociology_abstracts/cit_100_149/soc_cap/soc_cap_2_complete.csv")
soc_cap_100_149_3 <- read.csv("data/acs_sociology_abstracts/cit_100_149/soc_cap/soc_cap_3_complete.csv")
soc_cap_150_199_1 <- read.csv("data/acs_sociology_abstracts/cit_150_199/soc_cap/soc_cap_1_complete.csv")
soc_cap_150_199_2 <- read.csv("data/acs_sociology_abstracts/cit_150_199/soc_cap/soc_cap_2_complete.csv")
soc_cap_150_199_3 <- read.csv("data/acs_sociology_abstracts/cit_150_199/soc_cap/soc_cap_3_complete.csv")
soc_cap_200_99999_1 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/soc_cap/soc_cap_1_complete.csv")
soc_cap_200_99999_2 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/soc_cap/soc_cap_2_complete.csv")
soc_cap_200_99999_3 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/soc_cap/soc_cap_3_complete.csv")
soc_cap_complete <- rbind(soc_cap_1_49_0, soc_cap_1_49_1, soc_cap_1_49_2,soc_cap_1_49_3,
                          soc_cap_50_99_1,soc_cap_50_99_2,soc_cap_50_99_3,
                          soc_cap_100_149_1,soc_cap_100_149_2,soc_cap_100_149_3,
                          soc_cap_150_199_1,soc_cap_150_199_2,soc_cap_150_199_3,
                          soc_cap_200_99999_1,soc_cap_200_99999_2,soc_cap_200_99999_3)
soc_cap_complete$sample_percent <- "100"

### Samples by percentage (5-4000 citation liimits)
soc_cap_sample_5_3 <- read.csv("data/acs_sociology_abstracts/sample_5/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_5_4 <- read.csv("data/acs_sociology_abstracts/sample_5/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_10_3 <- read.csv("data/acs_sociology_abstracts/sample_10/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_10_4 <- read.csv("data/acs_sociology_abstracts/sample_10/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_10_5 <- read.csv("data/acs_sociology_abstracts/sample_10/soc_cap/soc_cap_5_complete.csv")
soc_cap_sample_15_3 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_15_4 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_15_5 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_5_complete.csv")
soc_cap_sample_15_6 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_6_complete.csv")
soc_cap_sample_15_7 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_7_complete.csv")
soc_cap_sample_15_8 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_8_complete.csv")
soc_cap_sample_15_9 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_9_complete.csv")
soc_cap_sample_15_10 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_10_complete.csv")
soc_cap_sample_15_11 <- read.csv("data/acs_sociology_abstracts/sample_15/soc_cap/soc_cap_11_complete.csv")
soc_cap_sample_20_3 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_20_4 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_20_5 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_5_complete.csv")
soc_cap_sample_20_6 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_6_complete.csv")
soc_cap_sample_20_7 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_7_complete.csv")
soc_cap_sample_20_8 <- read.csv("data/acs_sociology_abstracts/sample_20/soc_cap/soc_cap_8_complete.csv")
soc_cap_sample_25_3 <- read.csv("data/acs_sociology_abstracts/sample_25/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_25_4 <- read.csv("data/acs_sociology_abstracts/sample_25/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_30_3 <- read.csv("data/acs_sociology_abstracts/sample_30/soc_cap/soc_cap_3_complete.csv")
soc_cap_sample_30_4 <- read.csv("data/acs_sociology_abstracts/sample_30/soc_cap/soc_cap_4_complete.csv")
soc_cap_sample_5_3$sample_percent <- "5"
soc_cap_sample_5_4$sample_percent <- "5"
soc_cap_sample_10_3$sample_percent <- "10"
soc_cap_sample_10_4$sample_percent <- "10"
soc_cap_sample_10_5$sample_percent <- "10"
soc_cap_sample_15_3$sample_percent <- "15"
soc_cap_sample_15_4$sample_percent <- "15"
soc_cap_sample_15_5$sample_percent <- "15"
soc_cap_sample_15_6$sample_percent <- "15"
soc_cap_sample_15_7$sample_percent <- "15"
soc_cap_sample_15_8$sample_percent <- "15"
soc_cap_sample_15_9$sample_percent <- "15"
soc_cap_sample_15_10$sample_percent <- "15"
soc_cap_sample_15_11$sample_percent <- "15"
soc_cap_sample_20_3$sample_percent <- "20"
soc_cap_sample_20_4$sample_percent <- "20"
soc_cap_sample_20_5$sample_percent <- "20"
soc_cap_sample_20_6$sample_percent <- "20"
soc_cap_sample_20_7$sample_percent <- "20"
soc_cap_sample_20_8$sample_percent <- "20"
soc_cap_sample_25_3$sample_percent <- "25"
soc_cap_sample_25_4$sample_percent <- "25"
soc_cap_sample_30_3$sample_percent <- "30"
soc_cap_sample_30_4$sample_percent <- "30"
soc_cap_sample <- rbind(soc_cap_sample_5_3,soc_cap_sample_5_4,
                        soc_cap_sample_10_3,soc_cap_sample_10_4,soc_cap_sample_10_5,
                        soc_cap_sample_15_3,soc_cap_sample_15_4, soc_cap_sample_15_5,
                        soc_cap_sample_15_6,soc_cap_sample_15_7, soc_cap_sample_15_8,
                        soc_cap_sample_15_9,soc_cap_sample_15_10, soc_cap_sample_15_11,
                        soc_cap_sample_20_3,soc_cap_sample_20_4, soc_cap_sample_20_5,
                        soc_cap_sample_20_6,soc_cap_sample_20_7, soc_cap_sample_20_8,
                        soc_cap_sample_25_3,soc_cap_sample_25_4,
                        soc_cap_sample_30_3,soc_cap_sample_30_4)

### Combine complete citations with sample citations and save to csv
soc_cap_all <- rbind(soc_cap_complete,soc_cap_sample)
names(soc_cap_all)[names(soc_cap_all) == "cit_level"] <- "depth"
### Remove duplicates
duplicates_soc_cap <- duplicated(soc_cap_all[c(12,14,15)])
length(duplicates_soc_cap[duplicates_soc_cap== TRUE])
soc_cap_all <- soc_cap_all[!duplicated(soc_cap_all[c(12,14,15)]),]
write_excel_csv(soc_cap_all,"data/soc_cap_all.csv")

## "Emergence of scaling in random networks"
### Complete set of citations (1-99999 citation limits)
emergence_1_9_0 <- read.csv("data/acs_sociology_abstracts/cit_1_9/emergence/emergence_0_complete.csv")
emergence_1_9_1 <- read.csv("data/acs_sociology_abstracts/cit_1_9/emergence/emergence_1_complete.csv")
emergence_1_9_2 <- read.csv("data/acs_sociology_abstracts/cit_1_9/emergence/emergence_2_complete.csv")
emergence_10_19_1 <- read.csv("data/acs_sociology_abstracts/cit_10_19/emergence/emergence_1_complete.csv")
emergence_10_19_2 <- read.csv("data/acs_sociology_abstracts/cit_10_19/emergence/emergence_2_complete.csv")
emergence_20_29_1 <- read.csv("data/acs_sociology_abstracts/cit_20_29/emergence/emergence_1_complete.csv")
emergence_20_29_2 <- read.csv("data/acs_sociology_abstracts/cit_20_29/emergence/emergence_2_complete.csv")
emergence_30_39_1 <- read.csv("data/acs_sociology_abstracts/cit_30_39/emergence/emergence_1_complete.csv")
emergence_30_39_2 <- read.csv("data/acs_sociology_abstracts/cit_30_39/emergence/emergence_2_complete.csv")
emergence_40_49_1 <- read.csv("data/acs_sociology_abstracts/cit_40_49/emergence/emergence_1_complete.csv")
emergence_40_49_2 <- read.csv("data/acs_sociology_abstracts/cit_40_49/emergence/emergence_2_complete.csv")
emergence_50_99_1 <- read.csv("data/acs_sociology_abstracts/cit_50_99/emergence/emergence_1_complete.csv")
emergence_50_99_2 <- read.csv("data/acs_sociology_abstracts/cit_50_99/emergence/emergence_2_complete.csv")
emergence_100_149_1 <- read.csv("data/acs_sociology_abstracts/cit_100_149/emergence/emergence_1_complete.csv")
emergence_100_149_2 <- read.csv("data/acs_sociology_abstracts/cit_100_149/emergence/emergence_2_complete.csv")
emergence_150_199_1 <- read.csv("data/acs_sociology_abstracts/cit_150_199/emergence/emergence_1_complete.csv")
emergence_150_199_2 <- read.csv("data/acs_sociology_abstracts/cit_150_199/emergence/emergence_2_complete.csv")
emergence_200_99999_1 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/emergence/emergence_1_complete.csv")
emergence_200_99999_2 <- read.csv("data/acs_sociology_abstracts/cit_200_99999/emergence/emergence_2_complete.csv")

### Samples by percentage (5-4000 citation liimits)
emergence_sample_5_3 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_3_complete.csv")
emergence_sample_5_4 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_4_complete.csv")
emergence_sample_5_5 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_5_complete.csv")
emergence_sample_5_6 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_6_complete.csv")
emergence_sample_5_7 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_7_complete.csv")
emergence_sample_5_8 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_8_complete.csv")
emergence_sample_5_9 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_9_complete.csv")
emergence_sample_5_10 <- read.csv("data/acs_sociology_abstracts/sample_5/emergence/emergence_10_complete.csv")
emergence_sample_10_3 <- read.csv("data/acs_sociology_abstracts/sample_10/emergence/emergence_3_complete.csv")
emergence_sample_10_4 <- read.csv("data/acs_sociology_abstracts/sample_10/emergence/emergence_4_complete.csv")
emergence_sample_10_5 <- read.csv("data/acs_sociology_abstracts/sample_10/emergence/emergence_5_complete.csv")
emergence_sample_5_3$sample_percent <- "5"
emergence_sample_5_4$sample_percent <- "5"
emergence_sample_5_5$sample_percent <- "5"
emergence_sample_5_6$sample_percent <- "5"
emergence_sample_5_7$sample_percent <- "5"
emergence_sample_5_8$sample_percent <- "5"
emergence_sample_5_9$sample_percent <- "5"
emergence_sample_5_10$sample_percent <- "5"
emergence_sample_10_3$sample_percent <- "10"
emergence_sample_10_4$sample_percent <- "10"
emergence_sample_10_5$sample_percent <- "10"
emergence_sample <- rbind(emergence_sample_5_3,emergence_sample_5_4,emergence_sample_5_5,
                          emergence_sample_5_6,emergence_sample_5_7,emergence_sample_5_8,
                          emergence_sample_5_9,emergence_sample_5_10,
                          emergence_sample_10_3,emergence_sample_10_4,emergence_sample_10_5)

### Combine complete citations with sample citations and save to csv
emergence_all <- rbind(emergence_complete,emergence_sample)
names(emergence_all)[names(emergence_all) == "cit_level"] <- "depth"
### Remove duplicates
duplicates_emergence <- duplicated(emergence_all[c(12,14,15)])
length(duplicates_emergence[duplicates_emergence== TRUE])
emergence_all <- emergence_all[!duplicated(emergence_all[c(12,14,15)]),]
write_excel_csv(emergence_all,"data/emergence_all.csv")
