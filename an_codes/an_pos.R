# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) POS tagging
### Section B) textstat_frequency


rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)
library(quanteda)
library(ggplot2)
library(ggtext)
library(ggrepel)

load(file = "cr_data/news2.RData")

# Section A) Convert to Corpus and do POS tagging using SPACYR ####

## Convert to corpus ==== 
pos_corpus <- corpus(news2,
                     docid_field = "id",
                     text_field = "title")

## Spacyr to parse into POS ====
library("spacyr")
spacy_initialize(model = "en_core_web_sm") 

pos_parsed <- spacy_parse(pos_corpus)
head(pos_parsed)

## Keep to ADJ, VERB, NOUNS ====
pos_parsed <- 
    pos_parsed %>% 
    filter(pos == "ADJ"| pos == "VERB" | pos == "NOUN" | pos == "ADV")

## Adjective analysis ====
pos_adj <- 
    pos_parsed %>% 
    filter(pos == "ADJ") %>%  
    as.tokens()
    # convert to token object
# 
mystopwords <- c("man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

pos_adj_dfm <- 
    dfm(pos_adj,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) %>% 
    dfm_wordstem() 
# impt that word stemming comes after stop word removal. 

### Wordcloud with adjectives ====

library("quanteda.textplots")

set.seed(100)
textplot_wordcloud(pos_adj_dfm, 
                   max_words = 100, 
                   min_count = 10,
                   random_order = F, 
                   #  rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))  #

# trim down the dfm to those features less than n occurrences
pos_adj_dfm_reduced <- dfm_trim(pos_adj_dfm, min_termfreq = 10)
topfeatures(pos_adj_dfm_reduced) # top features
nfeat(pos_adj_dfm_reduced) # number of features 


library("quanteda.textplots")
pos_adj_fcm <- fcm(pos_adj_dfm_reduced)
dim(pos_adj_fcm)
head(pos_adj_fcm)
nfeat(pos_adj_fcm)

toptag <- names(topfeatures(pos_adj_fcm, 20))
head(toptag, n = 10)
size <- log(colSums(dfm_select(pos_adj_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(pos_adj_fcm, pattern = toptag)

set.seed(100)
textplot_network(top_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3, edge_color = "orange", edge_alpha = 5, edge_size = 2)


## Noun analysis ====
pos_noun <- 
    pos_parsed %>% 
    filter(pos == "NOUN") %>%  
    as.tokens()
# convert to token object
# 
mystopwords <- c("man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

pos_noun_dfm <- 
    dfm(pos_noun,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) %>% 
    dfm_wordstem() 
# impt that word stemming comes after stop word removal. 

### Wordcloud with adjectives ====

library("quanteda.textplots")

set.seed(100)
textplot_wordcloud(pos_noun_dfm, 
                   max_words = 100, 
                   min_count = 10,
                   random_order = F, 
                   #  rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))  #

# trim down the dfm to those features less than n occurrences
pos_noun_dfm_reduced <- dfm_trim(pos_noun_dfm, min_termfreq = 10)
topfeatures(pos_noun_dfm_reduced) # top features
nfeat(pos_noun_dfm_reduced) # number of features 


library("quanteda.textplots")
pos_noun_fcm <- fcm(pos_noun_dfm_reduced)
dim(pos_noun_fcm)
head(pos_noun_fcm)
nfeat(pos_noun_fcm)

toptag <- names(topfeatures(pos_noun_fcm, 20))
head(toptag, n = 10)
size <- log(colSums(dfm_select(pos_noun_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(pos_noun_fcm, pattern = toptag)

set.seed(100)
textplot_network(top_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3, edge_color = "orange", edge_alpha = 5, edge_size = 2)

