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

# How many dialogues?
length(ep4$dialogue)

# How many characters?
length(levels(ep4$character))

# Top 20 characters with more dialogues 
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]

# Visualization 
ggplot(data=top.ep4.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")


wordcloud2(frequentTerms(ep4$dialogue), size=0.5,
           figPath="vader.png")
wordcloud2(frequentTerms(ep4$dialogue))
gc()

# ep5$dialogue %>% VectorSource() %>% Corpus() %>% cleanCorpus() %>% 
#   TermDocumentMatrix() %>% removeSparseTerms(0.99) %>% as.matrix() %>% rowSums() %>% 
#   sort( decreasing=TRUE)

wordcloud2(combined,size=0.7,
           figPath="wattsworth.png")

# wordcloud2(demoFreq, 
#            figPath = "C:/Users/Owen/Projects/Star-Wars-Movie-Scripts/yoda.png", size = 1.5,color = "skyblue")

# How many dialogues?
length(ep6$dialogue)

# How many characters?
length(levels(ep6$character))

# Top 20 characters with more dialogues 
top.ep6.chars <- as.data.frame(sort(table(ep6$character), decreasing=TRUE))[1:20,]

# Visualization 
ggplot(data=top.ep6.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

# The Original Trilogy dialogues 
trilogy <- rbind(ep4, ep5, ep6)

# How many dialogues?
length(trilogy$dialogue)

# How many characters?
length(levels(trilogy$character))

# Top 20 characters with more dialogues 
top.chars <- as.data.frame(sort(table(trilogy$character), decreasing=TRUE))[1:20,]

# Visualization 
ggplot(data=top.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

wordcloud2(combined, size=0.4,
           figPath="rebel alliance.png")

tokens <- trilogy %>%  
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  unnest_tokens(word, dialogue)

tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n" , fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)

# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency")