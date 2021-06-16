# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) # of articles over time
### Section B) Save dataset for analysis in another do-file 


rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggtext)
library(ggrepel)

load(file = "cr_data/news2.RData")

# Generate indicator for fathers #####
  # will merge in this with the mum set for plotting of trends
  # "cr_data/news2_mum.RData"

news2 <-
  news2 %>% 
  mutate(mum = replicate(nrow(news2),1)) %>% 
  relocate(mum, .after=id)


# Plot number by year ####
num_by_yr <- 
    news2 %>%
    filter(year != 2021) %>% 
    group_by(year) %>% 
    summarize(num_yr = n()) %>% 
    ungroup()

num_by_yr_dad <- 
  news2 %>%
  filter(source == "ST" & year != 2021) %>% 
  group_by(year) %>% 
  summarize(num_yr = n()) %>% 
  ungroup()


# Save this to merge into mum set for plotting #####
  # This will be done in another dofile
  # file = "an_data/num_by_yr_mum.RData")

save(num_by_yr_dad, file = "an_data/num_by_yr_dad.RData")
rm(num_by_yr_dad)


# Plotting #####
p <- ggplot(data = num_by_yr, mapping = aes(x = year, y = num_yr )) 
p + geom_line(color= "#2a474b" , size = 1) +   #"steelblue"
    labs(title = "Number of Articles",
         subtitle = "1992 to 2020",
         x = "",
         y = "Number of articles each year",
         caption = "gerardchung.com | Codes: https://github.com/gerardchung/social_work_news_singapore") +
 #   scale_x_date(date_labels = "%Y", date_breaks = "2 year", limit=c(min, max)) +
    geom_smooth(color = "#ec9914" ,size =2,  method = "lm" , se = T) +  # "indianred3"
    scale_color_brewer(palette = "Dark2",  guide="none") +
    theme_minimal() 

p + geom_point() +
    labs(title = "Number of Articles",
         subtitle = "1992 to 2020",
         x = "",
         y = "Number of articles each year",
         caption = "gerardchung.com | Codes: https://github.com/gerardchung/social_work_news_singapore") +
    #   scale_x_date(date_labels = "%Y", date_breaks = "2 year", limit=c(min, max)) +
    geom_smooth(color = "#ec9914" ,size =2,  method = "lm" , se = T) +  # "indianred3"
    scale_color_brewer(palette = "Dark2",  guide="none") +
    theme_minimal() 

## Regression 

lm.year_articles = glm(num_yr~as.numeric(year), family="poisson", data = num_by_yr)
summary(lm.year_articles)

# Number of articles by source (ST, Today, CNA) #####

num_by_yr_source <- 
    news2 %>% 
    filter(year != 2021) %>% 
    group_by(year, source) %>% 
    summarize(num_yr = n()) %>% 
    ungroup()

p <- ggplot(data = num_by_yr_source, mapping = aes(x = year, y = num_yr , group = source, color = source))
p + geom_point(alpha = .6, size =1.5) +
    geom_smooth( size =1.2,  method = "lm" , se = F) +  # "indianred3"
    scale_color_brewer(palette = "Set1",  guide="none") +  #Dark2 
    scale_x_continuous(limits = c(1992, 2020), breaks = seq(1992, 2020, by = 4)) +
    
    labs(title = "Number of articles with 'father'-related phrases in titles",
         subtitle = "In <b><span style = 'color: #E41A1C;'>**Straits Times**</span></b>, <b><span style = 'color: #377EB8;'>**TodayOnline**</span></b>, and <b><span style = 'color: #4DAF4A;'>**CNA**</span></b> from 1992-2020", 
         x = "",
         y = "Number of articles",
         caption = c("@GerardChung | gerardchung.com | Codes: https://github.com/gerardchung/news_fathers", 
                     "Note: father-related phrases include dad*, father*, paternal")) +

  #  theme_minimal() + 
    theme_classic(base_family = "Roboto Condensed") + 
    theme(
        rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.title = ggtext::element_markdown(size = 15, face="bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "Roboto Condensed", size = 13, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 9, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20"),
        plot.caption = element_text(hjust=c(1, 0))) +  # this line of code is to have two captions
  
    ggrepel::geom_text_repel(data = num_by_yr_source %>% filter(source == "ST") %>%  filter(year %in% range(year)), 
                             aes(label =  num_yr ),  # paste0(Importance)
                             hjust = "left",
                             fontface = "plain",
                             family = "Roboto Condensed",
                             size = 3.5,
                             #   nudge_x = -.9,
                             direction = "y")   +
    # range will give the min and max values of year (you can change to num_by_yr_source)
                                 
    ggrepel::geom_text_repel(data = num_by_yr_source %>% filter(source == "Today") %>% filter(year %in% range(year)), 
                             aes(label =  num_yr ),  # paste0(Importance)
                             hjust = "left",
                             fontface = "plain",
                             family = "Roboto Condensed",
                             size = 3.5,
                          #   nudge_y = 1.5,nudge_x = 1,
                             direction = "y")  +
    ggrepel::geom_text_repel(data = num_by_yr_source %>% filter(source == "CNA") %>%  filter(year %in% range(year)), 
                             aes(label =  num_yr ),  # paste0(Importance)
                             hjust = "left",
                             fontface = "plain",
                             family = "Roboto Condensed",
                             size = 3.5,
                             nudge_x = .3,nudge_y = -.9,
                             direction = "y") 

# Colors from Set1 used in this graph
library("RColorBrewer")
brewer.pal(3,"Set1")
red = "#E41A1C"
blue = "#377EB8"
green = "#4DAF4A"

