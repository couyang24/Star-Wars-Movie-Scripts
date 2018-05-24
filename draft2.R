pacman::p_load(tidyverse, tm, wordcloud, wordcloud2, tidytext, reshape2)
library(tm)

# Read the data
ep4 <- read.table("SW_EpisodeIV.txt")
ep5 <- read.table("SW_EpisodeV.txt")
ep6 <- read.table("SW_EpisodeVI.txt")



# Wordcloud for Episode V
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive",
                                           "will","can","cant","dont","youve",
                                           "youre","youll","theyre","whats","didnt","us"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
  
}


frequentTerms <- function(text){
  
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.99)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}


combined <- frequentTerms(ep6$dialogue) %>% bind_rows(frequentTerms(ep6$dialogue)) %>% 
    bind_rows(frequentTerms(ep4$dialogue))

combined <- combined %>% mutate(freq = if_else(freq>35, freq+25, freq)) %>% arrange(desc(freq))

wordcloud2(combined, size=.5,
           figPath="vader.png")


# ep5$dialogue %>% VectorSource() %>% Corpus() %>% cleanCorpus() %>% 
#   TermDocumentMatrix() %>% removeSparseTerms(0.99) %>% as.matrix() %>% rowSums() %>% 
#   sort( decreasing=TRUE)

wordcloud2(combined,size=0.55,
           figPath="yoda.png")

# wordcloud2(demoFreq, 
#            figPath = "C:/Users/Owen/Projects/Star-Wars-Movie-Scripts/yoda.png", size = 1.5,color = "skyblue")
