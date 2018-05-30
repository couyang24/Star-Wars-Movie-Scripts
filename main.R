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




top_chars




clean_top_char <- function(dataset){
  all_dialogue <- list()
  namelist <- list()
  
  for (i in 1:10){
    
    name <- top_chars$character[i]
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







# common_words <- subset(all_clean, all_clean[, "LUKE"] > 0 & all_clean[, "THREEPIO"] > 0)
 


common_words_25 <- all_clean %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(LUKE>0, THREEPIO>0) %>% 
  # select(LUKE, THREEPIO) %>% 
  mutate(difference = abs(LUKE - THREEPIO)) %>% 
  arrange(desc(difference)) %>%
  head(25)




# Create the pyramid plot
pyramid.plot(common_words_25$LUKE, common_words_25$THREEPIO,
             labels = common_words_25$rowname, gap = 8,
             top.labels = c("LUKE", "Words", "THREEPIO"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)




















all_clean %>%
  as.data.frame() %>% 
  rownames_to_column(var = 'word') %>%
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  group_by(sentiment) %>% 
  summarise(number = sum(LUKE)) %>% 
  plot_ly(labels = ~sentiment, values = ~number) %>%
  add_pie(hole = 0.6)  %>%
  layout(title = "LUKE Emotions",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

all_clean %>%
  as.data.frame() %>% 
  rownames_to_column(var = 'word') %>%
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  group_by(sentiment) %>% 
  summarise(number = sum(THREEPIO)) %>% 
  plot_ly(labels = ~sentiment, values = ~number) %>%
  add_pie(hole = 0.6)  %>%
  layout(title = "THREEPIO Emotions",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



all_clean %>%
  as.data.frame() %>% 
  rownames_to_column(var = 'word') %>%
  inner_join(get_sentiments("loughran"), by = 'word') %>% 
  select(word, LUKE, sentiment) %>% 
  filter(LUKE!=0) %>% 
  spread(sentiment, LUKE, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4", "firebrick", "steelblue"), max.words=50)

all_clean %>%
  as.data.frame() %>% 
  rownames_to_column(var = 'word') %>%
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  select(word, VADER, sentiment) %>% 
  spread(sentiment, VADER, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=150)

  
senti_LUKE_THREE <- all_clean %>%
  as.data.frame() %>% 
  rownames_to_column(var = 'word') %>%
  inner_join(get_sentiments("nrc"), by = 'word')%>% 
  select(LUKE, THREEPIO, sentiment) %>% 
  group_by(sentiment) %>% 
  summarise(sum_luke = sum(LUKE),
            sum_threepio = sum(THREEPIO))

pyramid.plot(senti_LUKE_THREE$sum_luke, senti_LUKE_THREE$sum_threepio,
             labels = senti_LUKE_THREE$sentiment, gap = 30,
             top.labels = c("LUKE", "Sentiment", "THREEPIO"),
             main = "Sentiment Comparison", laxlab = NULL, 
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

clean_luke %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>% 
  spread(sentiment, Freq, fill = 0) %>% 
  column_to_rownames(var = 'word') %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=50)




















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



