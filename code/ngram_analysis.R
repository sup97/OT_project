ngram_analysis <- function(data, ngram){
  cleaned_dataset$text <- tokens(data$text, remove_puct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_url=TRUE) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_remove(keywords, padding  = TRUE) %>%
    tokens_remove(replace_reg, padding  = TRUE) %>%
    tokens_ngrams(n = ngram) %>%
    dfm()
  
  #textstat_lexdiv(cleaned_dataset$text)
  textstat_frequency(cleaned_dataset$text)
  xtable(tbl_df(topfeatures(cleaned_dataset$text)))
  #head(cleaned_dataset$text)
  freq <- tbl_df(textstat_frequency(cleaned_dataset$text)) #need frequency info
  head(freq)
  
  freq_t <- tbl_df(freq) %>%
    group_by(feature) %>%
    tally(frequency) %>%
    top_n(30)
  
  jpeg(paste("~/Box Sync/Dissertation/Analysis/Results", ngram,"gram.png"), width = 1000, height = 1500, res = 300)
  ggplot(freq_t, aes(x=reorder(feature,n), n))+
    geom_col()+
    xlab("word")+
    ylab("frequency")+
    coord_flip()
  dev.off()
}
