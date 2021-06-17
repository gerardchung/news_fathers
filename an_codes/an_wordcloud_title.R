# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) Convert to Corpus and DFM
### Section B) Word clouds
### Section C) Frequency of word occurrences


rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)
library(quanteda)
library(ggplot2)
library(ggtext)
library(ggrepel)

load(file = "cr_data/news2.RData")

# Section A) Convert to Corpus and DFM ####

## Convert to corpus ==== 
news_corpus <- corpus(news2,
                      docid_field = "id",
                      text_field = "title")

## Convert to tokens -> then DFM ==== 
news_tok <- 
    tokens(news_corpus, 
                remove_numbers = T,
                remove_punct = T,
                remove_symbols = T,
                include_docvars = T, 
                remove_separators = T
                ) 
#%>% 
#    tokens_wordstem() 
    
#news_tok <- tokens_tolower(news_tok)
#mystopwords <- c("father", "fathers", 
#                 "dad", "dads", "daddy", "daddies", 
#                 "man", "paternal", 
#                 "singapore", "s", "s'pore"
#                # "says",
#                # "two",
#                # "get",
#                # "one",
#                # "find",
#                # "found",
#                # "day"
#                 )
            # why "says", "s" etc? These came from the wordcloud in the following
            # and I exclude them since they are not useful to the meaning 

mystopwords <- c("father", "fathers", 
                 "dad", "dads", "daddy", "daddies", 
                 "man", "paternal", 
                 "singapore", "s", "s'pore", "says", "two", "get", "one", "find", "found", "day")

news_dfm <- 
    dfm(news_tok,
                tolower = T) %>% 
    dfm_remove(
               c(phrase(mystopwords), stopwords("en"))) %>% 
    dfm_wordstem() 
    # impt that word stemming comes after stop word removal. 
  

# Section B) Word Clouds ####
library("quanteda.textplots")
set.seed(100)
#textplot_wordcloud(news_dfm)

textplot_wordcloud(news_dfm, 
                   max_words = 100, 
                   min_count = 30,
                   random_order = F, 
                   #  rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))  #

set.seed(100)
textplot_wordcloud(news_dfm, color = c("#459DE0", "#F53446"), max_words = 100,
                   min_count = 20, random_order = F ) 


str_view(news2$title, pattern = "says")
str_view(news2$title, pattern = "\\bson\\b", match = T)
str_view(news2$title, pattern = "\\blike\\b", match = T)


# Section C) Frequency of word occurrences ####

## Frequency plots using textstat_frequency ==== 
# https://quanteda.io/articles/pkgdown/examples/plotting.html#frequency-plots-1
# https://tutorials.quanteda.io/statistical-analysis/frequency/
# https://quanteda.io/reference/textstat_frequency.html

library("quanteda.textstats")

## Colors from Set1 used in this graph ==== 
library("RColorBrewer")
brewer.pal(3,"Set1")
red = "#E41A1C"
blue = "#377EB8"
green = "#4DAF4A"

## Create DF for textstat_freq ==== 
features_plot <- textstat_frequency(news_dfm, n = 20)
features_plot$feature <- with(features_plot, reorder(feature, docfreq)) 
    # docfreq = how many docs the word appear in
    # freq = how many times the word appear (can be once or more in a doc)

## Plot the textstat_freq  ==== 
ggplot(features_plot, aes(x = feature, y = (docfreq))) +
    geom_segment( aes(xend=feature, yend=0)) +
    geom_point(color = blue, size = 3) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip() + 
    
    labs(title = "In news about fathers in Singapore, what are the top **co-occurring** words?",
         subtitle = "Top 20 words from 1992 to 2021",
         x = "Word",
         y = "Number of articles",
         caption = "Plot by **Gerard Chung** | gerardchung.com | Codes at **github.com/gerardchung/news_fathers**<br>Note: **'news'** include Straits Times, CNA, & TodayOnline") +
    
    theme_classic(base_family = "Roboto Condensed") + 
    theme(
        rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.title = ggtext::element_markdown(size = 16),
        plot.title.position = "plot",
        plot.subtitle = element_markdown( size = 14, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 12, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5 )) +
    
    ggrepel::geom_label_repel(data = features_plot %>% filter(feature == "son"), 
                             aes(label =  docfreq),  # paste0(Importance)
                             hjust = "left",
                             fontface = "plain",
                             family = "Roboto",
                             size = 3,
                              nudge_x = -0.7, 
                             box.padding = 0.01,
                             fill = "#F5F5F5",
                             direction = "y",
                             
                            ) +
    ggrepel::geom_label_repel(data = features_plot %>% filter(feature == "daughter"), 
                             aes(label =  docfreq),  # paste0(Importance)
                             hjust = "left", 
                             fontface = "plain",
                             family = "Roboto",
                             size = 3,
                             nudge_x = -.4,nudge_y = 5.2, box.padding = 0.01, segment.curvature = 0.08,
                             fill = "#F5F5F5",
                            direction = "y") -> freqplot
  

## See samples of titles for the top words ==== 
str_view(news2$title, pattern = "\\bson\\b", match = T)
son_text <- news2
son_text$son <- str_detect(son_text$title, regex(pattern = "\\bson\\b", ignore_case = T))

dg_text <- news2
dg_text$daughter <- str_detect(dg_text$title, regex(pattern = "\\bdaughter\\b", ignore_case = T))

son_text <-
  son_text %>% 
  filter(son == T) %>% 
  select(id, date, source, title, text, son )

head(son_text$title, n = 10) 
print(son_text$title) 

dg_text <-
  dg_text %>% 
  filter(daughter == T) %>% 
  select(id, date, source, title, text, daughter )

head(dg_text$title, n = 10) 
print(dg_text$title) 

## Sample text 1 
son_text[son_text$title=="Commando son part of Best Combat Unit - just like his  dad",]
# 2017-06-29 Todayonline
## Sample text 2
son_text[son_text$title=="Father  v son: 2; Dad , 96, wants entire compensation given to son, 70, for Hock Kee unit",]
## Sample text 3
son_text[son_text$title=="Father  and son among 109 drug offenders arrested in islandwide raids",]
## Sample text 4
son_text[son_text$title=="Like  father , like son, like grandsons",]
## Sample text 5
son_text[son_text$title=="Father  and son are like friends; Actor-host and single  dad  Jeff Wang says he spends most of his free time with his 13-year-old son",]




freqplot + 
  scale_y_continuous(expand = c(0,2)) + # make words nearer to start of lines
  annotate("text", x=18, y=210, 
           label= "'Commando son part of Best 
           Combat Unit - just like his dad' 
           - Straits Times, 2005-11-26",
           size = 3.5,
           family = "Roboto Condensed") +
  annotate("text", x=15, y=210, 
           label= "'Like father, like son, like grandsons' 
           - ST, 2006-01-15",
           size = 3.5,
           family = "Roboto Condensed")  +
  annotate("text", x=12, y=210, 
           label= "'Father v son: Dad, 96, wants entire 
           compensation given to son, 70, for Hock Kee unit' 
           - TodayOnline, 2017-06-09",
           size = 3.5,
           family = "Roboto Condensed") + 
  annotate("text", x=8, y=200, 
           label= "'Father and son among 109 drug offenders 
           arrested in islandwide raids' 
           - Channel News Asia, 2019-07-12",
           size = 3.5,
           family = "Roboto Condensed") + 
  annotate("text", x=4, y=200, 
           label= "'Father and son are like friends; 
           Actor-host and single dad Jeff Wang says 
           he spends most of his free time 
           with his 13-year-old son' 
           - ST, 2021-05-31",
           size = 3.5,
           family = "Roboto Condensed") -> plot_final
  # https://viz-ggplot2.rsquaredacademy.com/ggplot2-text-annotations.html

ggsave("plots/2_topoccurringwords.png", plot = plot_final, type = 'cairo', width = 9, height = 6.5, dpi = 300, units = "in", bg = "#F5F5F5")

 
# Feature-occurrence matrix ######
# https://quanteda.io/articles/pkgdown/examples/twitter.html
# https://tutorials.quanteda.io/basic-operations/fcm/fcm/

# trim down the dfm to those features less than n occurrences
news_dfm_reduced <- dfm_trim(news_dfm, min_termfreq = 50)
topfeatures(news_dfm_reduced) # top features
nfeat(news_dfm_reduced) # number of features 


library("quanteda.textplots")
news_fcm <- fcm(news_dfm_reduced)
dim(news_fcm)
head(news_fcm)
nfeat(news_fcm)

toptag <- names(topfeatures(news_fcm, 20))
head(toptag, n = 10)
size <- log(colSums(dfm_select(news_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(news_fcm, pattern = toptag)

set.seed(100)
textplot_network(top_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3, edge_color = "orange", edge_alpha = 5, edge_size = 2)

str_view(news2$title, pattern = "\\btwo\\b", match = T) # two is not a useful word to keep
str_view(news2$title, pattern = "\\bget\\b", match = T) # get is not a useful word to keep
str_view(news2$title, pattern = "\\bnew\\b", match = T) # new is USEFUL: new fathers
str_view(news2$title, pattern = "\\bfind\\b", match = T) # find is not a useful word to keep
str_view(news2$title, pattern = "\\bwant\\b", match = T) # want is not a useful word to keep
str_view(news2$title, pattern = "\\btime\\b", match = T) # time is USEFUL:
str_view(news2$title, pattern = "\\bone\\b", match = T) # one is not a useful word to keep
str_view(news2$title, pattern = "\\bday\\b", match = T) # day is not a useful word to keep



textplot_network(top_fcm, min_freq = 1, edge_alpha = 0.5, edge_size = 5)
textplot_network(top_fcm, min_freq = 0.01, edge_color = "orange", edge_alpha = 0.8, edge_size = 1)

