pacman::p_load(tidyverse, tm, plotly, highcharter, viridis, 
               wordcloud, wordcloud2, plotrix, tidytext,
               reshape2)

ep4 <- read.table("input/SW_EpisodeIV.txt")
ep5 <- read.table("input/SW_EpisodeV.txt")
ep6 <- read.table("input/SW_EpisodeVI.txt")



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



ep4$dialogue %>% 
  frequentTerms() %>% 
  # dim()
  head(30) %>% 
  mutate(word = factor(word))%>% 
  plot_ly(x = ~reorder(word,-freq), y = ~freq, colors = viridis(10)) %>%
  add_bars(color = ~word) %>%
  layout(title = "Top 10 Words", 
         yaxis = list(title = " "), 
         xaxis = list(title = "Words"), 
         margin = list(l = 100))





# Top 20 characters with more dialogues 
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]

hchart(top.ep4.chars, type = 'treemap',hcaes(x = "Var1", value = 'Freq', color = 'Freq'))

all_luke <- paste(ep4$dialogue[ep4$character == 'LUKE'], collapse = " ")
  
all_vader <- paste(ep4$dialogue[ep4$character == 'VADER'], collapse = " ")

all_dialogue <- c(all_luke, all_vader)

all_clean <- all_dialogue %>% 
  VectorSource() %>% 
  Corpus() %>% 
  cleanCorpus() %>% 
  TermDocumentMatrix() %>%
  as.matrix()
  
colnames(all_clean) <- c("LUKE","VADER")

commonality.cloud(all_clean, colors = "steelblue1", at.least = 2, max.words = 100)

comparison.cloud(all_clean, colors = c("#F8766D", "#00BFC4"), max.words=50)







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
  group_by(sentiment) %>% 
  summarise(number = sum(Freq))
  
clean_vader %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=50)










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
  group_by(sentiment) %>% 
  summarise(number = sum(Freq))






















get_sentiments("bing")



# Word association
word_associate(ep4$dialogue[ep4$character == 'VADER'], match.string = c("rebel"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Vader Rebel Comment")







# Word association
word_associate(ep4$dialogue, match.string = c("vader"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Vader Rebel Comment")



