library(tidytext)
library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)
library(ggplot2)
library(igraph)
library(ggraph)
library(textreadr)
library(pdftools) # pdf_text
library(magrittr) # scrapping websites
library(rvest) # scrapping websites
library(twitteR) # twitter
library(tm) # twitter
library(magrittr) # Scraping wesites from text
library(rvest) # Scraping wesites from text
library(dplyr) # group_by, tidy
library(tidyr)
library(tidytext) #stop_words, tidy
library(tidyverse)
library(stringr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(textdata)
library(tm) # for DTM
library(Matrix)
library(scales)
library(igraph)
library(ggraph)

reviews <- read_document(file="/Users/ayarapontioso/Desktop/getout_imdb.docx")
get_out <- c(reviews)

a <- 169
b <- 2

getout_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    getout_df[i,z]<- get_out[i*b+z-b]
  }
}

names(getout_df)[1] <- 'Ratings'
names(getout_df)[2] <- 'User Reviews'
View(getout_df)

#####Ratings
ratings <- getout_df$`Ratings`
ratings_df <- data_frame(line=1:a, text=ratings)
print(ratings_df)

data("stop_words")
ratingstoken <- ratings_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)
print(ratingstoken)

ratings_nrc <- get_sentiments('nrc')
nrc_ratings <- ratingstoken %>%
  inner_join(ratings_nrc)
print(nrc_ratings)

ratings_ngrams <- ratings_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)

ratings_ngrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- ratings_ngrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigrams_filtered %>%
  filter(n>2) %>%
  graph_from_data_frame()
bigram_graph

ggraph(bigrams_counts, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



#####User Reviews
userreviews <- getout_df$`User Reviews`
userreviews_df <- data_frame(line=1:a, text=userreviews)
print(userreviews_df)

data("stop_words")
userreviewstoken <- userreviews_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)
print(userreviewstoken)

userreviews_nrc <- get_sentiments('nrc')
nrc_userreviews <- userreviewstoken %>%
  inner_join(userreviews_nrc)
print(nrc_userreviews)

tidy_userrreviews <- userreviews_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_userrreviews %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Sentiment", x=NULL)+
  coord_flip()

tidy_userrreviews %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey10", "gray90"),
                   max.words=100)

getout_df <- bind_rows(
  mutate(ratingstoken, question = 'Ratings'),
  mutate(userreviewstoken, question = 'User Reviews'))
getout_df <- getout_df %>%
  bind_tf_idf(word, question, n)
getout_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()
