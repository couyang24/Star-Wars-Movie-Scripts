pacman::p_load(tidyverse, tm, plotly, highcharter, viridis, wordcloud, wordcloud2)

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

# Top 20 characters with more dialogues 
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]


ep4$dialogue %>% 
  frequentTerms() %>% 
  # dim()
  head(10) %>% 
  mutate(word = factor(word))%>% 
  plot_ly(x = ~reorder(word,-freq), y = ~freq, colors = viridis(10)) %>%
  add_bars(color = ~word) %>%
  layout(title = "Top 10 Words", 
         yaxis = list(title = " "), 
         xaxis = list(title = "Words"), 
         margin = list(l = 100))



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
  
c("LUKE","VADER") -> colnames(all_clean)


commonality.cloud(all_clean, colors = "steelblue1", at.least = 2, max.words = 100)

comparison.cloud(all_clean, colors = c("#F8766D", "#00BFC4"), max.words=40)

