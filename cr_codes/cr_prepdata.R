# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) Clean vars
### Section B) Extract data 
### Section C) Create dataset for analysis in another do-file 

rm(list=ls())

library(stringr)
library(dplyr)
library(janitor)

load(file = "cr_data/news.RData")


# SECTION A) CLEAN VARIABLES #####

## Foldernum ====
news$foldernum <- as.numeric(news$foldernum)

## Source ====
unique(news$source)

### Today
news$source2 <-  replace(news$source2, news$source =="Today (Singapore) - Online" , "Today")
news$source2 <-  replace(news$source2, news$source ==("TODAY (Singapore)") , "Today")

### ST
news$source2 <-  replace(news$source2, news$source =="The Straits Times (Singapore)" , "ST")
news$source2 <-  replace(news$source2, news$source =="The Straits Times" , "ST")

### CNA
news$source2 <-  replace(news$source2, news$source =="Channel NewsAsia" , "CNA")

unique(news$source)
unique(news$source2)
tabyl(news$source)

news$source2 <- factor(news$source2, levels = c("ST", "Today", "CNA"), labels = c("ST", "Today", "CNA"))
tabyl(news$source2, exclude = F)

news <- 
    news %>% 
    mutate(source = source2) %>% 
    select(-source2)

## pub.date #####


### month
months <- c("January", "February", "March", "April", 
            "May", "June", "July", "August", 
            "September", "October", "November", "December")
news$month <- str_extract(news$pub.date, pattern = paste(months,collapse = "|"))
unique(news$month)

news <- 
    news %>% 
    relocate(month, .after = pub.date)
    
news$month <- factor(news$month, levels = months, labels = months)

tabyl(news$month)

### year 
str_view(news$pub.date, regex(pattern = "\\d{4}"))
news <- 
    news %>% 
    mutate(year = str_extract(pub.date,
                                  regex(pattern = "\\d{4}"))) %>% 
    relocate(year, .before = month)

tabyl(news$year)
news$year <- as.numeric(news$year)

### day 
str_view(news$pub.date, regex(pattern = "\\d{1,2}"))

news <- 
    news %>% 
    mutate(day = str_extract(pub.date,
                                  regex(pattern = "\\d{1,2}"))) %>% 
    relocate(day, .after =   month)

tabyl(news$day)
news$day <- as.numeric(news$day)

### Combine year, month, and day into a date variable 

# Convert to Date using lubricate
library(tidyverse)
library(lubridate)

news <- 
    news %>%
    mutate(date = make_date(year, month, day)) %>% 
    relocate(date, .before = pub.date) %>% 
    select(-pub.date)
tabyl(year(news$date))
tabyl(month(news$date))
tabyl(day(news$date))


# FINAL SECTION) save as Rdata file #####
getwd()
save(news, file = "cr_data/news.RData") 
load(file = "cr_data/news.RData")

