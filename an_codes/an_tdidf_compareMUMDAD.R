# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) Convert to Corpus and DFM
### Section B) Word clouds
### Section C) Frequency of word occurrences


rm(list=ls())

library(stringr)
library(dplyr)
library(tidyverse)
library(janitor)
library(quanteda)
library(ggplot2)
library(ggtext)
library(ggrepel)

# DAD's corpus #####

## Load dad's corpus =====
load(file = "cr_data/news2.RData")



## Convert to Corpus and DFM =====

### Convert to corpus ==== 
news_corpus <- corpus(news2,
                      docid_field = "id",
                      text_field = "title")


### Convert to tokens =====
dad_tok <- 
    tokens(news_corpus, 
           remove_numbers = T,
           remove_punct = T,
           remove_symbols = T,
           include_docvars = T, 
           remove_separators = T
    ) 
    
### Stop words =====
mystopwords <- c("father", "fathers", 
                 "dad", "dads", "daddy", "daddies", 
                 "man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day", "year*", "year")


### Convert to dfm ====
dad_dfm <- 
    dfm(dad_tok,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("english"))) %>% 
    dfm_wordstem() 


## Calculate term-freq inverse doc freq using dfm_tfidf function ====

### Calculate tdidf using dfm_tfidf  function
td_idf_dad <- 
    dad_dfm %>% 
    dfm_tfidf() 

topfeatures(td_idf_dad[1,]) # this is for each document




### extact features with highest tdidf and convert into dataframe ====
td_idf_dad_DF <- 
    td_idf_dad %>% 
    topfeatures( n = 20) %>% 
    as.data.frame()

### Rename variables and save dad datafrae =====

td_idf_dad_DF <-rename(td_idf_dad_DF, td_idf = `.`)

topfeatures(td_idf_dad, n = 20)

td_idf_dad_DF$features <- c("son","family","jail","daughter","mum",
                            "death","kid","die","girl","new","like","help",
                            "time","parent","children","life","home","baby","work", "take")

td_idf_dad_DF <- 
    td_idf_dad_DF %>% 
    mutate(group = replicate(nrow(td_idf_dad_DF),1))

rm(dad_dfm,dad_tok,news2,td_idf_dad)


# MUM's corpus #####

## Load um's corpus =====
load(file = "cr_data/news2_mum.RData")


## Convert to Corpus and DFM =====

### Convert to corpus ==== 
news_corpus <- corpus(news2_mum,
                      docid_field = "id",
                      text_field = "title")


### Convert to tokens =====
mum_tok <- 
    tokens(news_corpus, 
           remove_numbers = T,
           remove_punct = T,
           remove_symbols = T,
           include_docvars = T, 
           remove_separators = T
    ) 

### Stop words =====
mystopwords <- c("mother", "mothers", 
                 "mum", "mums", "mummy", "mummies", 
                 "woman", "maternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", 
                 "find", "found", "day", "year*")


### Convert to dfm ====
mum_dfm <- 
    dfm(mum_tok,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("english"))) %>% 
    dfm_wordstem()


## Calculate term-freq inverse doc freq using dfm_tfidf function ====

### Calculate tdidf using dfm_tfidf  function
td_idf_mum <- 
    mum_dfm %>% 
    dfm_tfidf() 


### extact features with highest tdidf and convert into dataframe ====
td_idf_mum_DF <- 
    td_idf_mum %>% 
    topfeatures( n = 20) %>% 
    as.data.frame()

### Rename variables and save dad datafrae =====
td_idf_mum_DF <-rename(td_idf_mum_DF, td_idf = `.`)
topfeatures(td_idf_mum, n = 20)

td_idf_mum_DF$features <- c("son","baby","help","daughter","jail","family",
                            "kid","work","new","man","boy","children",
                            "dad","girl","single","home","keep","life","death", "child"
)


td_idf_mum_DF <- 
    td_idf_mum_DF %>% 
    mutate(group = replicate(nrow(td_idf_mum_DF),2))
           
rm(mum_dfm,mum_tok,news2_mum,td_idf_mum)


# COMBINE BOTH DAD AND MUM'S TDIDF DATAFRAMES #######

## Combine both data-frames ==== 
tdfif_combine <- bind_rows(td_idf_dad_DF, td_idf_mum_DF)

tdfif_combine$group <- factor(tdfif_combine$group, levels = c(1,2), labels = c("dad","mum"))

rm(td_idf_dad_DF, td_idf_mum_DF)


# Plot #####

## Create diff ======
tdfif_combine <- 
    tdfif_combine %>% 
    pivot_wider(names_from = group, values_from = td_idf) %>% 
    mutate(diff = dad-mum) %>% 
    pivot_longer(cols = c(dad,mum)) %>% 
    rename(Gender = name,
           Enrollments = value)

## Select non-missing  ======
tdfif_combine <-
    tdfif_combine %>% 
    filter(!is.na(diff))
    
## Select the features i want ====
    # top five features of dad excluding mum
features_i_want = c("son","family","jail","daughter","death")
tdfif_combine <- 
    tdfif_combine %>% 
    filter(features %in% features_i_want)
    
tdfif_combine$features <- factor(tdfif_combine$features, 
                                 levels = c("son","family",
                                            "jail","daughter","death"))

## Prep for plotting 

dat_gender <- tdfif_combine

dat_gender %>%
    group_by(Gender) %>%
    summarise(mean = mean(Enrollments),
              SE = sd(Enrollments)) %>%
    mutate(meanpos = mean + 1 *SE,
           meanneg = mean - 1 *SE)-> stats


stats_males <- stats %>%
    filter(Gender == "dad")

stats_females <- stats %>%
    filter(Gender == "mum")

Males <- dat_gender %>%
    filter(Gender == "dad")

Females <- dat_gender %>%
    filter(Gender == "mum")

diff <- dat_gender %>%
    filter(Gender == "dad") %>%
    mutate(x_pos = Enrollments + (abs(diff)/2))

green = "#009688"
purple = "#762a83"
# Colors from Set1 used in this graph
library("RColorBrewer")
brewer.pal(3,"Set1")
red = "#E41A1C"
darkred = "#96000f"
blue = "#377EB8"
green = "#4DAF4A"

library(ggtext)
library(extrafont)

ggplot(dat_gender, aes(x = Enrollments, y = reorder(factor(features),Enrollments))) +
    
    geom_segment(data = Males, aes(x = Enrollments, y = features, yend = Females$features,  xend = Females$Enrollments),
                 color = "#aeb6bf", size = 4.5, alpha = .5) +
    geom_point( aes(color = Gender), size = 4.5, show.legend = FALSE) +
    scale_color_manual(values = c(blue,darkred)) +
    
    geom_text(data = diff %>% filter(features != "son" & features != "death"), 
              aes(label = paste("∆",sprintf("%0.1f", diff)), x = x_pos, y = features), 
               color = "#4a4e4d", size = 2.5, family = "Roboto") +
    
    geom_text_repel(data = diff %>% filter(features == "son"), 
              aes(label = paste("∆",sprintf("%0.1f", diff)), x = x_pos-2*(abs(diff)/2), y = features), # becos x_pos was ref. from dad
              color = "#4a4e4d", size = 2.5, family = "Roboto",
              nudge_y = 0.1,
              direction = "y") +
    
    geom_text(data = diff %>% filter(features == "death"), 
              aes(label = paste("∆",sprintf("%0.1f", diff)), x = x_pos+5, y = features), # becos x_pos was ref. from dad
              color = "#4a4e4d", size = 2.5, family = "Roboto") +
    
    facet_grid(features ~ ., scales = "free", switch = "y") + # the words on yaxis comes from here; the axis.text.y turns it off
    xlab("Keyness") + ylab("Word") +
    
    ggtitle("Key words in fathers' related articles")+
    labs(subtitle = "Top key words for <span style = 'color: #377EB8;'>**Father**</span> articles compared to <span style = 'color: #96000f;'>**Mother**</span><br>",
         caption = "Plot by **Gerard Chung** | gerardchung.com | Codes at **github.com/gerardchung/news_fathers**<br>Note: **Keyness** is measured using term-freq inverse doc-freq metric") +


    theme_minimal()+
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color = "#4a4e4d"),
          text = element_text(family = "Roboto Condensed", color = "#4a4e4d"),
          strip.text.y.left  = element_text(angle = 0, size = 13), 
          panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
          strip.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
          strip.text = element_text(color = "#4a4e4d", family = "Roboto Condensed"),
          plot.background = element_rect(fill = "#F5F5F5"),
          panel.spacing = unit(0, "lines"),
          plot.margin = margin(1,1,.5,1, "cm"),
          plot.caption = element_markdown(hjust = 0, lineheight = 1.5, ),
          plot.subtitle = element_markdown(size = 14, hjust = -.23),
          plot.title = element_text(size = 16, hjust = -.17, face = "bold")) 

  

    