# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) Convert to Corpus and DFM
### Section B) Prep for Key ATM and get key word list 
### Section C) Run final model with covariates 


rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)
library(quanteda)

load(file = "cr_data/news2.RData")



# Section A) Convert to Corpus and DFM ####

## Convert to corpus ==== 
news_corpus <- corpus(news2,
                      docid_field = "id",
                      text_field = "text")

## Convert to tokens -> then DFM ==== 
news_tok <- 
    tokens(news_corpus, 
           remove_numbers = T,
           remove_punct = T,
           remove_symbols = T,
           include_docvars = T, 
           remove_separators = T
    ) 

mystopwords <- c("father", "fathers", 
                 "dad", "dads", "daddy", "daddies", 
                 "man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

news_dfm <- 
    dfm(news_tok,
        tolower = T) %>% 
    dfm_remove(
        c(phrase(mystopwords), stopwords("en"))) %>% 
    dfm_wordstem() %>% 
    dfm_trim(min_termfreq = 5, min_docfreq = 2)
    # remove words with les than 5 and occurs in less than 2 docs
    # I follow from https://keyatm.github.io/keyATM/articles/pkgdown_files/Preparation.html 
    # impt that word stemming comes after stop word removal. 

# Keyatm preparations #####
library(keyATM)

## Reads data into KeyATM ====
keyATM_docs <- keyATM_read(texts = news_dfm)
summary(keyATM_docs) # there are 30 dos with zero tokens

### remove docs with zero words
news_dfm <- dfm_subset(news_dfm, ntoken(news_dfm) > 0)
keyATM_docs <- keyATM_read(texts = news_dfm) # read in again 
summary(keyATM_docs) # no docs now with zero tokens 

## Prep key words =====
keywords <- list(son     = c("sons", "son", "boy", "boys"),
                 daughter       = c("daughter", "daughters"),
                 death          = c("die", "died", "death", "deaths", "dead"),
                 jail   = c("jail", "jails", "prison", "convict", "charged"),
                 mum = c("mum", "mother", "mummy", "wife", "wives"),
                 parenting = c("parent", "parenting", "child", "newborn", "fathering", "fatherhood", "baby", "newborn"))

key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keywords)
key_viz # 
values_fig(key_viz) 


## Unsupervised to get key words =====

#set.seed(225)  # set the seed before split the dfm
#docs_withSplit <- keyATM_read(texts = news_dfm,
#                              split = 0.3)  # split each document
#
#out <- weightedLDA(docs              = docs_withSplit$W_split,  # 30% of the corpus
#                   number_of_topics  = 10,  # the number of potential themes in the corpus 
#                   model             = "base",
#                   options           = list(seed = 250))
#top_words(out)  # top words can aid selecting keywords
#top_docs(out)
#
#news2$title[news2$id==1451]
#news2$title[news2$id==676]
#news2$title[news2$id==1684]
#
#news2$title[news2$id==555]
#news2$title[news2$id==894]
#news2$title[news2$id==627]
#
#news2$title[news2$id==1853]
#news2$title[news2$id==1228]
#news2$title[news2$id==124]
#news2$title[news2$id==2009]
#
#news2$title[news2$id==1489]
#news2$title[news2$id==1153]
#news2$title[news2$id==233]
#news2$title[news2$id==906]
#news2$title[news2$id==375]
#
#news2$title[news2$id==1912]
#news2$title[news2$id==1743]
#news2$title[news2$id==1462]
#news2$title[news2$id==1542]
#news2$title[news2$id==761]
#
#news2$title[news2$id==1622]
#news2$title[news2$id==1699]
#news2$title[news2$id==2066]
#news2$title[news2$id==1142]
#news2$title[news2$id==1038]
#
#news2$title[news2$id==149]
#news2$title[news2$id==666]
#news2$title[news2$id==957]
#news2$title[news2$id==1776]
#
#news2$title[news2$id==1953]
#news2$title[news2$id==1353]
#news2$title[news2$id==472]
#news2$title[news2$id==1614]
#
#news2$title[news2$id==1104]
#news2$title[news2$id==511]
#news2$title[news2$id==1196]
#news2$title[news2$id==136]
#
#news2$title[news2$id==833]
#news2$title[news2$id==6]
#news2$title[news2$id==234]
#news2$title[news2$id==1755]
#
#keywords2 <- list(son     = c("sons", "son", "boy", "boys"),
#                  daughter       = c("daughter", "daughters"),
#                  death          = c("die", "died", "death", "deaths", "dead"),
#                  jail   = c("jail", "jails", "prison", "convict", "charged"),
#                  mum = c("mum", "mother", "mummy", "wife", "wives"),
#                  parenting = c("famili", "children", "help", "work", "parent", "parenting", "new", "child", "newborn", #"fathering", "baby", "newborn"),
#                  arts_music = c("play", "music", "film", "movi", "perform"),
#                  politics = c("lee", "minist", "polit", "presid", "parti", "govern", "founding"),
#                  culinary = c("cook", "food", "dish", "restaur", "chef", "eat"),
#                  father_day = c("day", "celebrate"),
#                  business = c("busi", "compani", "invest", "money", "million", "firm"),
#                  crime_general = c("report", "polic", "jail"),
#                  child_abuse = c("abuse",  "baby", "sexual", "caning","report", "polic", "jail", "protection"))
#
#
### 
#
#out <- keyATM(docs              = docs_withSplit,  # 70% of the corpus
#              no_keyword_topics = 5,               # number of topics without keywords
#              keywords          = keywords2,        # selected keywords
#              model             = "base",          # select the model
#              options           = list(seed = 250))
#
#
## 
#
#### choosing keywords with unsupervised model ======
##out <- keyATM(docs              = keyATM_docs,    # text input
##              no_keyword_topics = 5,              # number of topics without keywords
##              keywords          = keywords,       # keywords
##              model             = "base",         # select the model
##              options           = list(seed = 250))
#
### save the fitted model =====
#save(out, file = "an_data/keyatm/keyatm_fathernews.rds")
##out <- readRDS(file = "an_data/keyatm/keyatm_fathernews.rds")
#top_words(out)
#top_docs(out)
#plot_pi(out)
#
#news2$title[news2$id==1023]
#news2$title[news2$id==1923]
#news2$title[news2$id==399]
#news2$title[news2$id==814]
#
#
#news2$title[news2$id==1108]
#news2$title[news2$id==1704]
#news2$title[news2$id==436]
#news2$title[news2$id==1104]
#
#news2$title[news2$id==1142]
#news2$title[news2$id==1344]
#news2$title[news2$id==436]
#news2$title[news2$id==1104]
#
#news2$title[news2$id==651]
#news2$title[news2$id==1548]
#
#news2$title[news2$id==1492]
#news2$title[news2$id==1584]

## After the prior work to dev the list of key words =======

## Final key word list ======
keywords3 <- list(son     = c("sons", "son", "boy", "boys"),
                  daughter       = c("daughter", "daughters"),
                  death          = c("die", "died", "death", "deaths", "dead"),
                  crime   = c("jail", "jails", "prison", "convict", "charged"),
                  mum = c("mum", "mother", "mummy", "wife", "wives"),
                  fathering = c("famili", "children", "help", "work", "parent", "parenting", "new", "child", "newborn", "fathering", "baby", "newborn"),
                  arts_music = c("play", "music", "film", "movi", "perform"),
                  politics = c("lee", "minist", "polit", "presid", "parti", "govern", "founding"),
                  culinary = c("cook", "food", "dish", "restaur", "chef", "eat"),
                  business = c("busi", "compani", "invest", "money", "million", "firm"),
                  child_abuse = c("abuse",  "baby", "sexual", "caning","report", "polic", "jail", "protection"),
                  ns_armedforces = c("servic", "forc", "nation", "offic", "camp", "ns"),
                  sports = c("game", "coach", "team", "footbal", "sport", "train", "player"))

## Run the final model (without covariates) ======
#out3 <- keyATM(docs              = docs_withSplit,  # 70% of the corpus
#              no_keyword_topics = 3,               # number of topics without keywords
#              keywords          = keywords3,        # selected keywords
#              model             = "base",          # select the model
#              options           = list(seed = 250))
#
#save(out3, file = "an_data/keyatm/keyatm_fathernews3.rds")
#top_words(out3)
#top_docs(out3)
#plot_pi(out3)
#
#news2$title[news2$id==996]
#news2$title[news2$id==900]
#news2$title[news2$id==1154]
#news2$title[news2$id==2073]
#
## Final
#out <- keyATM(docs              = keyATM_docs,    # text input
#              no_keyword_topics = 5,              # number of topics without keywords
#              keywords          = keywords3,       # keywords
#              model             = "base",         # select the model
#              options           = list(seed = 250))
##save(out, file = "an_data/keyatm/keyatm_fathernews_final.rds")

#top_words(out)
#top_docs(out)
#plot_pi(out)

# Section C) Run the final model With covariates model  ##### 
vars <- docvars(news_corpus)
head(vars)

library(dplyr)
vars %>%
    as_tibble() %>%
    mutate(Period = case_when(year <2000 ~ "1990s",
                              year >=2000 & year <=2009 ~ "2000s",
                              TRUE ~ "2010s_2020s"
                              ),
           Period2 = case_when(year <2009 ~ "1990s_2000s",
                               TRUE ~ "2010s_2020s"
           )) %>%
    select(Period, Period2) -> vars_selected
table(vars_selected) # i will use Period2

#out_cov <- keyATM(docs              = keyATM_docs,
#              no_keyword_topics = 3,
#              keywords          = keywords3,
#              model             = "covariates",
#              model_settings    = list(covariates_data    = vars_selected, 
#                                       covariates_formula = ~ Period2),
#              options           = list(seed = 250))

top_words(out_cov)
top_docs(out_cov)  



## Save the mode
#save(out_cov, file = "an_data/keyatm/keyatm_fathernews_final.rds")
#save(out_cov, file = "an_data/keyatm/keyatm_fathernews_final2.rds")
load(file = "an_data/keyatm/keyatm_fathernews_final.rds")
rm(keyATM_docs, news_dfm, news_tok)

## sample aarticles 
# fathers
top_words(out_cov)
top_docs(out_cov)  
news2$title[news2$id==1501]
news2$title[news2$id==1270]
news2$title[news2$id==474]
news2$title[news2$id==503]
news2$title[news2$id==468]
news2$title[news2$id==1353] ##
news2$title[news2$id==1718] ##
news2$title[news2$id==1482]

print(news2[news2$id==1353,]) ##
print(news2[news2$id==1718,]) ##

# Abuse 
news2$title[news2$id==1033]
news2$title[news2$id==833]
news2$title[news2$id==814]
news2$title[news2$id==1203]
news2$title[news2$id==399]

print(news2[news2$id==399,]) ##
print(news2[news2$id==1033,]) ##

# ns_armforces 
news2$title[news2$id==1998]
news2$title[news2$id==94]
news2$title[news2$id==756]
news2$title[news2$id==613]
news2$title[news2$id==1788]

print(news2[news2$id==613,]) ##
print(news2[news2$id==1998,]) 

# Sports 
news2$title[news2$id==2045]
news2$title[news2$id==1176]
news2$title[news2$id==1370]
news2$title[news2$id==2066]
news2$title[news2$id==1967]

print(news2[news2$id==1967,]) 

# Business 
news2$title[news2$id==1382]
news2$title[news2$id==957]
news2$title[news2$id==1338]
news2$title[news2$id==455]
news2$title[news2$id==23]
news2$title[news2$id==944]
news2$title[news2$id==1770]
news2$title[news2$id==2064]
news2$title[news2$id==80]

print(news2[news2$id==1967,]) 
print(news2[news2$id==944,]) 

# death 
news2$title[news2$id==387]
news2$title[news2$id==432]
news2$title[news2$id==1848]
news2$title[news2$id==774]
news2$title[news2$id==889]
news2$title[news2$id==1195]
news2$title[news2$id==1944]
news2$title[news2$id==746]
news2$title[news2$id==887]

str_view(news2$title, pattern = "\\bdeath\\b", match = T)


print(news2[news2$id==150,]) 
print(news2[news2$id==944,]) 

## Get the covariates names ====
covariates_info(out_cov)
used_covariates <- covariates_get(out_cov)
head(used_covariates)
                                   
strata_topic <- by_strata_DocTopic(out_cov, by_var = "Period22010s_2020s",
                                   labels = c( "1990s_2000s", "2010s_2020s"))

fig_doctopic <- plot(strata_topic, var_name = "Period", show_topic = c(4, 6, 11,9))
fig_doctopic

## PLot ====
strata_topic_2020s <- by_strata_DocTopic(out_cov, by_var = "Period22010s_2020s",
                                   labels = c( "1990s_2000s", "2010s_2020s"))
est_2020s <- summary(strata_topic_2020s)[["2010s_2020s"]] # get the values out 
est_1990s <- summary(strata_topic_2020s)[["1990s_2000s"]] # get the values out 

# Rbind both dataframes 
# Combine both data-frames #####
est_all <- bind_rows(est_2020s, est_1990s)

est_all$label <- factor(est_all$label, levels = c("1990s_2000s","2010s_2020s"), labels = c("1990s to 2000s","2010s to 2020s"))

# plot 
tabyl(est_all$Topic)
topics = c("11_child_abuse", "6_fathering", "9_culinary", "12_ns_armedforces", "10_business")
topics = c("11_child_abuse", "6_fathering")
topics = c("11_child_abuse", "6_fathering", "9_culinary", "12_ns_armedforces")


library(ggplot2)
library(ggtext)

### Color theme for all the graphs =====
blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"
red = "#F53446"
darkred = "#96000f"
green = "#4DAF4A"



color_point <- c( darkred,green, blue , orange)
color_line <- "#7C878EFF"

p <- 
    est_all %>% 
    filter(Topic %in% topics) %>% 
    ggplot( aes(x = label , y = Point))


p + geom_line(aes(group = Topic), size = 2, alpha = .5, color = color_line) +
    geom_point(aes(color = Topic), size = 6, alpha = .7) +
    scale_color_manual(values = color_point) +
    labs(title = "How likely would the news about fathers in Singapore<br> be about <b><span style = 'color: #6892C1;'>**parenting**</span></b>, <b><span style = 'color: #96000f;'>**child abuse**</span></b>, <b><span style = 'color: #4DAF4A;'>**culinary**</span></b>, or <b><span style = 'color: #ED713F;'>**NS/Army**</span></b>?",
         subtitle = "Comparing the probabilities between two time periods",
         y = "Probability",
         x = "",
         caption = "Plot by **Gerard Chung** | gerardchung.com<br> Codes at github.com/gerardchung/news_fathers<br>
         News include Straits Times, TodayOnline, & CNA from 1992 to 2021"
         ) +
    guides(color = FALSE, size = FALSE) + 
    theme_classic(base_family = "Roboto Condensed") + 
    theme(
        rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.title = ggtext::element_markdown(size = 16, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "Roboto Condensed", size = 14, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 11, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10, color = "gray20"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5)
    ) ->  plot_final
    ggsave("plots/1_topicslikelihood.png", plot = plot_final, type = 'cairo', width = 6, height = 6.5, dpi = 300, units = "in", bg = "#F5F5F5")

