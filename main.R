pacman::p_load(tidyverse, tm, plotly, highcharter, viridis, 
               wordcloud, wordcloud2, plotrix, tidytext,
               reshape2, ggthemes, qdap)


ep4 <- read.table("input/SW_EpisodeIV.txt")
ep5 <- read.table("input/SW_EpisodeV.txt")
ep6 <- read.table("input/SW_EpisodeVI.txt")


combined <- bind_rows(ep4, ep5, ep6)
rm(ep4, ep5, ep6)

# Wordcloud for Episode V
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
  
}

# Most frequent terms 
frequentTerms <- function(text){
  
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}



combined$dialogue %>% 
  frequentTerms() %>% 
  # dim()
  head(30) %>% 
  mutate(word = factor(word))%>% 
  plot_ly(x = ~reorder(word,-freq), y = ~freq, colors = viridis(10)) %>%
  add_bars(color = ~word) %>%
  layout(title = "Top 30 Words", 
         yaxis = list(title = " "), 
         xaxis = list(title = ""), 
         margin = list(l = 100))






top_chars <- combined$character %>% as_data_frame() %>%  count(value) %>% arrange(desc(n)) %>% head(20)

hchart(top_chars, type = 'treemap',hcaes(x = "value", value = 'n', color = 'n'))









clean_top_char <- function(dataset){
  all_dialogue <- list()
  namelist <- list()
  
  for (i in 1:10){
    
  name <- top_chars$value[i]
  dialogue <- paste(dataset$dialogue[dataset$character == name], collapse = " ")
  all_dialogue <- c(all_dialogue, dialogue)
  namelist <- c(namelist, name)
  
  }
  
  
  
  all_clean <- all_dialogue %>% 
    VectorSource() %>% 
    Corpus() %>% 
    cleanCorpus() %>% 
    TermDocumentMatrix() %>%
    as.matrix()
    
  colnames(all_clean) <- namelist
  
  assign("all_clean",all_clean,.GlobalEnv)
  all_clean %>% head()
}

clean_top_char(combined)


commonality.cloud(all_clean[,c("LUKE","THREEPIO")], colors = "steelblue1", at.least = 2, max.words = 100)

comparison.cloud(all_clean[,c("LUKE","THREEPIO")], colors = c("#F8766D", "#00BFC4"), max.words=50)







common_words <- subset(all_clean, all_clean[, 1] > 0 & all_clean[, 2] > 0)

difference <- abs(common_words[, 1] -common_words[, 2])

# Combine common_words and difference
common_words <- cbind(common_words, difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top25_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, gap = 8,
             top.labels = c("LUKE", "Words", "VADER"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)
















clean_vader <- all_vader %>% 
  VectorSource() %>% 
  Corpus() %>% 
  cleanCorpus() %>% 
  TermDocumentMatrix() %>%
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column()

colnames(clean_vader) <- c("word","Freq")

clean_vader %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  count('sentiment')
  
clean_vader %>% 
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4", "firebrick", "steelblue"), max.words=50)

(a <- clean_vader %>% 
  inner_join(get_sentiments("nrc"), by = 'word') %>% 
    count('sentiment'))

(a_new <- a %>% 
  plot_ly(labels = ~sentiment, values = ~freq) %>%
  add_pie(hole = 0.6)  %>%
  layout(title = "Vader Emotions",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
get_sentiments("loughran") %>% select(sentiment) %>% table()
  
ggplotly(ggplot(a, aes(reorder(sentiment, -n), n, fill = sentiment)) +
  geom_col() +
  theme_economist() +
  scale_fill_brewer(palette="Spectral"))








clean_luke <- all_luke %>% 
  VectorSource() %>% 
  Corpus() %>% 
  cleanCorpus() %>% 
  TermDocumentMatrix() %>%
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column()

colnames(clean_luke) <- c("word","Freq")

clean_luke %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  count('sentiment')


clean_luke %>% 
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4", "firebrick", "steelblue"), max.words=50)


(b <- clean_luke %>% 
    inner_join(get_sentiments("nrc"), by = 'word') %>% 
    count('sentiment'))

b %>% 
  plot_ly(labels = ~sentiment, values = ~freq) %>%
  add_pie(hole = 0.6)  %>%
  layout(title = "Luke Emotions",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))












<<<<<<< HEAD
clean_luke %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=50)
=======

library(plotly)
p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>%
  add_lines(name = ~"unemploy")
p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>%
  add_lines(name = ~"uempmed")
p <- subplot(p1, p2)
>>>>>>> a10b6c5aa23dccbc4ecef92521f8c1de889180de






combined <- a %>% 
  full_join(b, by = 'sentiment')

a_new <- combined %>% 
  plot_ly(labels = ~sentiment, values = ~n.x) %>%
  add_pie(hole = 0.6, name = ~'n.x')

b_new <- combined %>% 
  plot_ly(labels = ~sentiment, values = ~n.y) %>%
  add_pie(hole = 0.6, name = ~'n.y') 

subplot(a_new, b_new, nrows = 1)








combined <- a %>% 
  full_join(b, by = 'sentiment')


pyramid.plot(combined$freq.x, combined$freq.y,
             labels = combined$sentiment, gap = 12,
             top.labels = c("LUKE", "Words", "VADER"),
             main = "Sentiment Comparison", laxlab = NULL, 
             raxlab = NULL, unit = NULL)











get_sentiments("bing")










# Word association
word_associate(combined$dialogue, match.string = c("yoda"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Master Yoda")


# Word association
word_associate(combined$dialogue[combined$character == 'VADER'], match.string = c("rebel"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Vader Rebel Comment")























