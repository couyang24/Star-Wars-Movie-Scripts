library(visNetwork)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tm, plotly, highcharter, viridis, 
               wordcloud, wordcloud2, plotrix, tidytext,
               reshape2, ggthemes, qdap, igraph, ggraph,
               visNetwork)

ep4 <- read.table("input/SW_EpisodeIV.txt")
ep5 <- read.table("input/SW_EpisodeV.txt")
ep6 <- read.table("input/SW_EpisodeVI.txt")

combined <- bind_rows(ep4, ep5, ep6)
rm(ep4, ep5, ep6)


# clean corpus
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                      "will","can","cant","dont","youve","us",
                                      "youre","youll","theyre","whats","didnt"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
}

# frequent terms 
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

# clean by each character
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


top_chars <- combined %>% count(character) %>% arrange(desc(n)) %>% head(20)

hchart(top_chars, type = 'treemap',hcaes(x = "character", value = 'n', color = 'n'))



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

clean_top_char(combined)

commonality.cloud(all_clean[,c("LUKE","THREEPIO")], colors = "steelblue1", at.least = 2, max.words = 100)

comparison.cloud(all_clean[,c("LUKE","THREEPIO")], colors = c("#F8766D", "#00BFC4"), max.words=50)



common_words <- all_clean %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(LUKE>0, THREEPIO>0) %>% 
  # select(LUKE, THREEPIO) %>% 
  mutate(difference = abs(LUKE - THREEPIO)) %>% 
  arrange(desc(difference)) 

common_words_25 <- common_words%>%
  head(25)


# Create the pyramid plot
pyramid.plot(common_words_25$LUKE, common_words_25$THREEPIO,
             labels = common_words_25$rowname, gap = 20,
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
             labels = senti_LUKE_THREE$sentiment, gap = 40,
             top.labels = c("LUKE", "Sentiment", "THREEPIO"),
             main = "Sentiment Comparison", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

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


char <- colnames(all_clean) %>% tolower()


all_clean_dt <- all_clean %>% 
as.data.frame() %>% 
select(-10)

colnames(all_clean_dt) <- colnames(all_clean_dt) %>% tolower()

network <- all_clean_dt[rownames(all_clean) %in% char,]

network_matrix <- network %>% select(rownames(network)) %>% as.matrix()

network1=graph_from_adjacency_matrix(network_matrix, mode='undirected', diag=F)

# plot it
plot(network1,                
# === vertex
vertex.color = rgb(0.8,0.4,0.3,0.8),          # Node color
vertex.frame.color = "white",                 # Node border color
vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
vertex.size=54,                               # Size of the node (default is 15)
vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
# Character vector used to label the nodes
vertex.label.color="white",
vertex.label.family="Times",                   # Font family of the label (e.g.“Times”, “Helvetica”)
vertex.label.font=c(1,2,3,4),                  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
vertex.label.cex=c(0.7,1,1.3),                 # Font size (multiplication factor, device-dependent)
vertex.label.dist=0,                           # Distance between the label and the vertex
vertex.label.degree=0,                         # The position of the label in relation to the vertex (use pi)ze2=NA)                               # The second size of the node (e.g. for a rectangle)
layout=layout.circle, main="Star Wars Relationship Network")                       


network2 <- network %>% 
  rownames_to_column('word1') %>% 
  gather(word2, value, -1)


network2 %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = value), show.legend = FALSE) +
  geom_node_point(color = "firebrick", size = 20, alpha = .5) +
  geom_node_text(aes(label = name), col = "white") +
  theme_solarized(light = F)


network3_edg <- network2 %>% 
  filter(value>5) %>% 
  rename(from = word1, to = word2, weight = value)

network3_node <- network2 %>% 
  group_by(word1) %>% 
  summarise(size = sum(value)) %>% 
  rename(id = word1)
network3_node$label <- network3_node$id # Node label

visNetwork(network3_node, network3_edg, height = "500px", width = "100%") %>% 
  visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))
