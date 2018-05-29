all_LEIA <- paste(combined$dialogue[combined$character == 'LEIA'], collapse = " ")

clean_LEIA <- all_LEIA %>% 
  VectorSource() %>% 
  Corpus() %>% 
  cleanCorpus() %>% 
  TermDocumentMatrix() %>%
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column()

colnames(clean_LEIA) <- c("word","Freq")

clean_LEIA %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  count('sentiment')


clean_LEIA %>% 
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4", "firebrick", "steelblue"), max.words=50)


(b <- clean_LEIA %>% 
    inner_join(get_sentiments("bing"), by = 'word') %>% 
    count('sentiment'))

b %>% 
  plot_ly(labels = ~sentiment, values = ~freq) %>%
  add_pie(hole = 0.6)  %>%
  layout(title = "LEIA Emotions",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
