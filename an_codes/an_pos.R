# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) POS tagging
### Section B) Adjective Analysis 
### Section C) Noun Analysis 


rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)
library(quanteda)
library(ggplot2)
library(ggtext)
library(ggrepel)

library("RColorBrewer")
brewer.pal(3,"Set1")
red = "#E41A1C"
blue = "#377EB8"
green = "#4DAF4A"

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

# Section B) Adjective analysis ####
# look at adjectives but keep father-related phrase to see what adjectives used to describe fathers

pos_adj <- 
    pos_parsed %>% 
    filter(pos == "NOUN" | pos == "ADJ") %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdad\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdads\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddies\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddy", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapa\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapas\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfathers\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfatherâ\\b", ignore_case = T), "father")) %>% 
    filter(pos == "ADJ" | token == "father") %>% 
    as.tokens()
        # filter NOUN and ADJ 
        # Why NOUN? So that i can replace dad* and papa* with father
        # I then filter to ADJ and only those noun that are father
        # Convert to tokens


## stop words ====
#mystopwords <- c("man", "paternal", 
#                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

mystopwords <- c("man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", 
                 "one", "find", "found", "day", "year", "years", "old", "-")

## Convert to DFM and do normalization ====
pos_adj_dfm <- 
    dfm(pos_adj,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) 

#%>% 
#    dfm_wordstem() 
# impt that word stemming comes after stop word removal. 

## Get ready for plot ====
library("quanteda.textplots")


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

## Plot ====
set.seed(100)
textplot_network(top_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3, edge_color = "orange", edge_alpha = 5, edge_size = 2)

## Check texts =====
head(toptag, n = 15)
str_view(news2$title, pattern = "\\bnew\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\blate\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bfirst\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bnational\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\blast\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bspecial\\b", match = T) # two is not a useful word to keep


rm(pos_adj, pos_adj_dfm, pos_adj_dfm_reduced, pos_adj_fcm) # remove objects

# Section C) Noun analysis #####

## Spacyr parsing ====
pos_noun <- 
    pos_parsed %>% 
    filter(pos == "NOUN") %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdad\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdads\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddies\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddy", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapa\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapas\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfathers\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfatherâ\\b", ignore_case = T), "father")) %>%  
    as.tokens()
        # Replace synonyms with fathers 
        # need the //b boundaries for word because daddy will become fatherdy
        # there was a word that is fatherâ -> so this is a error in the raw text


## convert to token object ====
mystopwords <- c("man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", 
                 "one", "find", "found", "day", "year", "years"
                 )
    # year is not useful because articles like to report age of the individual

pos_noun_dfm <- 
    dfm(pos_noun,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) 
#%>% 
#    dfm_wordstem() 
    # impt that word stemming comes after stop word removal. 
    # I did not do stemming

## Get ready for plotting ====

library("quanteda.textplots")

## trim down the dfm to those features less than n occurrences ====
pos_noun_dfm_reduced <- dfm_trim(pos_noun_dfm, min_termfreq = 50)
topfeatures(pos_noun_dfm_reduced) # top features
nfeat(pos_noun_dfm_reduced) # number of features 


pos_noun_fcm <- fcm(pos_noun_dfm_reduced)
dim(pos_noun_fcm)
head(pos_noun_fcm)
nfeat(pos_noun_fcm)

toptag <- names(topfeatures(pos_noun_fcm, 30))
head(toptag, n = 10)
size <- log(colSums(dfm_select(pos_noun_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(pos_noun_fcm, pattern = toptag)

## Plot ====
set.seed(100)
textplot_network(top_fcm, min_freq = 1, vertex_size = size / max(size) * 3, edge_color = "lightblue", edge_alpha = 5, edge_size = 2)

set.seed(100)
textplot_network(top_fcm, min_freq = 1,  edge_color = blue, edge_alpha = .5, edge_size = 2.5,
                 vertex_labelsize = 7,
                 vertex_labelfont = "Roboto Condensed",
                 vertex_size = 2) +
    labs(title = "Nouns that highly co-occurred with 'father'",
         caption = c("@GerardChung | gerardchung.com | Codes: https://github.com/gerardchung/news_fathers", 
                     "Note: father-related phrases include dad*, father*, paternal")) +
    theme(
        plot.caption = element_text(hjust=c(1, 0.01), vjust=c(1.0, 1.1)),
        plot.title = element_text(family = "Roboto Condensed", size = 14, hjust = 0.05, vjust = 0.05),
        )

#set.seed(100)
#options(ggrepel.max.overlaps = Inf)
#textplot_network(top_fcm, min_freq = 1,  edge_color = blue, edge_alpha = .5, edge_size = 2,
#                 vertex_labelsize = rowSums(top_fcm)/min(rowSums(top_fcm)),
#                 vertex_labelfont = "Roboto Condensed",
#                 vertex_size = 2) +
#    labs(title = "test")
#

str_view(news2$title, pattern = "\\byear\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bdeath\\b", match = T) 
str_view(news2$title, pattern = "\\bmum\\b", match = T) 

rm(pos_noun, pos_noun_dfm, pos_noun_dfm_reduced, pos_noun_fcm)

# Section D) Verbs analysis ####
# look at verbs but keep father-related phrase to see what adjectives used to describe fathers

pos_verb <- 
    pos_parsed %>% 
    filter(pos == "NOUN" | pos == "VERB") %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdad\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdads\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddies\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bdaddy", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapa\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bpapas\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfathers\\b", ignore_case = T), "father")) %>% 
    mutate(token = str_replace_all(token,regex(pattern = "\\bfatherâ\\b", ignore_case = T), "father")) %>% 
    filter(pos == "VERB" | token == "father") %>% 
    as.tokens()
         # filter VERB and ADJ 
         # Why Verb? So that i can replace dad* and papa* with father
         # I then filter to VERB and only those noun that are father
         # Convert to tokens


## stop words ====
#mystopwords <- c("man", "paternal", 
#                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

mystopwords <- c("man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", 
                 "one", "find", "found", "day", "year", "years", "old", "-", 
                 "gets", "wants", "takes", "founding")

## Convert to DFM and do normalization ====
pos_verb_dfm <- 
    dfm(pos_verb,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) 

#%>% 
#    dfm_wordstem() 
# impt that word stemming comes after stop word removal. 

## Get ready for plot ====
library("quanteda.textplots")


# trim down the dfm to those features less than n occurrences
pos_verb_dfm_reduced <- dfm_trim(pos_verb_dfm, min_termfreq = 30)
topfeatures(pos_verb_dfm_reduced) # top features
nfeat(pos_verb_dfm_reduced) # number of features 


pos_verb_fcm <- fcm(pos_verb_dfm_reduced)
dim(pos_verb_fcm)
head(pos_verb_fcm)
nfeat(pos_verb_fcm)

toptag <- names(topfeatures(pos_verb_fcm, 40))
head(toptag, n = 10)
size <- log(colSums(dfm_select(pos_verb_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(pos_verb_fcm, pattern = toptag)


## Plot ====
set.seed(100)
textplot_network(top_fcm, min_freq = 1, vertex_size = size / max(size) * 3, edge_color = "lightblue", edge_alpha = 5, edge_size = 2)

set.seed(100)
textplot_network(top_fcm, min_freq = 1,  edge_color = red, edge_alpha = .3, edge_size = 2.5,
                 vertex_labelsize = 7,
                 vertex_labelfont = "Roboto Condensed",
                 vertex_size = 2) +
    labs(title = "Verbs that highly co-occurred with 'father'",
         caption = c("@GerardChung | gerardchung.com | Codes: https://github.com/gerardchung/news_fathers", 
                     "Note: father-related phrases include dad*, father*, paternal")) +
    theme(
        plot.caption = element_text(hjust=c(1, 0.01), vjust=c(1.0, 1.1)),
        plot.title = element_text(family = "Roboto Condensed", size = 14, hjust = 0.05, vjust = 0.05),
    )


## Check texts =====
head(toptag, n = 15)

str_view(news2$title, pattern = "\\bfounding\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\btake\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bkilled\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bdies\\b", match = T) # two is not a useful word to keep


rm(pos_verb, pos_verb_dfm, pos_verb_dfm_reduced, pos_verb_fcm) # remove objects




