# Clean title columns
stop_words_en <- c(stopwords("en"))
## soc_cap
soc_cap_all$title <- stripWhitespace(removeNumbers(removePunctuation(tolower(soc_cap_all$title))))
soc_cap_all$title <- gsub("[[:punct:]]", "", soc_cap_all$title)
soc_cap_all$title <- removeWords(soc_cap_all$title, stop_words_en)
## diff_lim_agg
diff_lim_agg_all$title <- stripWhitespace(removeNumbers(removePunctuation(tolower(diff_lim_agg_all$title))))
diff_lim_agg_all$title <- gsub("[[:punct:]]", "", diff_lim_agg_all$title)
diff_lim_agg_all$title <- removeWords(diff_lim_agg_all$title, stop_words_en)
## emergence
emergence_all$title <- stripWhitespace(removeNumbers(removePunctuation(tolower(emergence_all$title))))
emergence_all$title <- gsub("[[:punct:]]", "", emergence_all$title)
emergence_all$title <- removeWords(emergence_all$title, stop_words_en)
