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
news2 <- news
rm(news)


# SECTION A) CLEAN VARIABLES #####

## Create an indicator variable #####
news2 <- 
    news2 %>% 
    mutate(id = row_number()) %>% 
    relocate(id, .before = foldernum)

## Foldernum ====
news2$foldernum <- as.numeric(news2$foldernum)

## Source ====
unique(news2$source)

### Today
news2$source2 <-  replace(news2$source2, news2$source =="Today (Singapore) - Online" , "Today")
news2$source2 <-  replace(news2$source2, news2$source ==("TODAY (Singapore)") , "Today")

### ST
news2$source2 <-  replace(news2$source2, news2$source =="The Straits Times (Singapore)" , "ST")
news2$source2 <-  replace(news2$source2, news2$source =="The Straits Times" , "ST")

### CNA
news2$source2 <-  replace(news2$source2, news2$source =="Channel NewsAsia" , "CNA")

unique(news2$source)
unique(news2$source2)
tabyl(news2$source)

news2$source2 <- factor(news2$source2, levels = c("ST", "Today", "CNA"), labels = c("ST", "Today", "CNA"))
tabyl(news2$source2, exclude = F)

news2 <- 
    news2 %>% 
    mutate(source = source2) %>% 
    select(-source2)

## pub.date #####


### month
months <- c("January", "February", "March", "April", 
            "May", "June", "July", "August", 
            "September", "October", "November", "December")
news2$month <- str_extract(news2$pub.date, pattern = paste(months,collapse = "|"))
unique(news2$month)

news2 <- 
    news2 %>% 
    relocate(month, .after = pub.date)
    
news2$month <- factor(news2$month, levels = months, labels = months)

tabyl(news2$month)

### year 
str_view(news2$pub.date, regex(pattern = "\\d{4}"))
news2 <- 
    news2 %>% 
    mutate(year = str_extract(pub.date,
                                  regex(pattern = "\\d{4}"))) %>% 
    relocate(year, .before = month)

tabyl(news2$year)
news2$year <- as.numeric(news2$year)

### day 
str_view(news2$pub.date, regex(pattern = "\\d{1,2}"))

news2 <- 
    news2 %>% 
    mutate(day = str_extract(pub.date,
                                  regex(pattern = "\\d{1,2}"))) %>% 
    relocate(day, .after =   month)

tabyl(news2$day)
news2$day <- as.numeric(news2$day)

### Combine year, month, and day into a date variable 

# Convert to Date using lubricate
library(tidyverse)
library(lubridate)

news2 <- 
    news2 %>%
    mutate(date = make_date(year, month, day)) %>% 
    relocate(date, .before = pub.date) %>% 
    select(-pub.date)
tabyl(year(news2$date))
tabyl(month(news2$date))
tabyl(day(news2$date))


## section ####
### see if can distinguish forum section
### In today news2, it is called "voices"

head(news2$section, n = 20)

str_view_all(news2$section, regex(pattern = "voice|\\bview\\b|opinion", ignore_case =T), match = T)
#str_view_all(news2$section, regex(pattern = "voice|view|opinion", ignore_case =T), match = T)
news2$voice <- str_detect(news2$section, regex(pattern = "voice|\\bview\\b|opinion", ignore_case =T))
sum(news2$voice, na.rm = T) # n =2
view_voice <- filter(news2, voice == T)
view_voice[2] # Only "voice" are forum letters

# detect for pattern in section with either word "forum" or "letter"
str_view_all(news2$section, regex(pattern = "forum|letter|voice", ignore_case =T), match = T)
news2$section_forum <- str_detect(news2$section, regex(pattern = "forum|letter|voice", ignore_case =T))
news2$section_forum <-    as.numeric(news2$section_forum) # change from logical to numeri

table(news2$section_forum, exclude = F)

# detect for a new pattern in text since "i refer" and "i commend" in text => forum letters
pattern.refer.commend = "I commend|We commend|I refer|We refer|with reference"
section1 <- news2[str_detect(news2$text, regex(pattern = pattern.refer.commend, ignore_case =T)),]
View_section1 <- section1 %>% select(text, section, section_forum)
# we can replace the empty values with T or 1 if there is this pattern in the text
# note that section = Voice can also be a forum letter
# six articles are detected using this new pattern

forum_pattern1 <- str_detect(news2$text, regex(pattern = pattern.refer.commend, ignore_case =T))
table(news2$section_forum, exclude = F)

news2 <- news2 %>% 
    mutate(section_forum = replace(section_forum, forum_pattern1 == T, 1))

table(news2$section_forum, exclude = F) # 96 are forum/ letters

table(news2$section_forum)
news2$section_forum <- factor(news2$section_forum, labels = c("articles", "forum_letters"))

View_section_forum <- 
    news2 %>% 
    select(id, section_forum, section, text, title)
#View(View_section_forum)
table(news2$section_forum)

news2 <- 
    news2 %>% 
    mutate(section_forum = 
               case_when(title == "Appeals court reverses care and control of boy from mum to  dad ; It found child's best interests would be better served with  father  here than with mother in UK" ~ "articles",
                         title == "Giving Hope to more families; Help scheme extended to families with jailed  dads  and may include divorced mums
" ~ "articles",
                         title == "Hush  Daddy" ~ "articles",
                         title == "Mahathir is a leader and  father  to me: Anwar" ~ "articles",
                         TRUE ~ as.character(section_forum)))
table(news2$section_forum)

rm(View_section_forum, View_section1, view_voice, section1)

news2 <- 
    news2 %>% 
    mutate(section_forum = case_when(is.na(section_forum)  ~ "articles",
                                      TRUE ~ as.character(section_forum)
                                      ))

news2$section_forum <- factor(news2$section_forum, labels = c("articles", "forum_letters"))
tabyl(news2$section_forum)



# FINAL SECTION) save as Rdata file #####

news2 <- 
    news2 %>% 
    select(-voice)

getwd()
save(news2, file = "cr_data/news2.RData") 

