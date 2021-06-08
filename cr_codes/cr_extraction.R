# NEWS ON FATHERS IN SINGAPORE 
# Text-mining news in Singapore abt fathers in Singapore == 

## Aim of do-file
### Section A) Create file names 
### Section B) Extract data 
### Section C) REMOVE ARTICLES THAT ARE DUPLICATES, OR NON-RELEVANT

rm(list = ls())

#renv::init()
#install.packages("textreadr")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("htmltools")
#install.packages("htmlwidgets")
#install.packages("janitor")

library(textreadr)
library(stringr)
library(dplyr)
library(janitor)

# SECTION A: CREATE FILES NAMES #### 
getwd()
# First, let's create a list of the files, which is the same first step we've seen before. However, here we can just use your computer's file structures and R commands to do so. 

# Keep in mind a few things that can happen:
#  there might be hidden files in the folder: do shift+command+dot to reveal hidden file. 
#  There is one in folder 1 "1/~$h, a social worker. I manually remove this file from the folder. 


## Extract file names ==== 
file.list = list.files("source_data", pattern = "\\D", recursive = T) # recursive will perform in each directory 
#file.list = list.files("source_data_nexis", recursive = T) # recursive will perform in each directory 
head(file.list)


## Remove those with "original_data" pattern
#file.list = str_subset(file.list, pattern = "original_data", negate = T) 
#file.list = str_subset(file.list, pattern = "1/~$h, a social worker", negate = T)


## Remove those files with doc_list =====
### hese are files that database Lexis Academic created to summarize the articles
sum(str_detect(file.list, pattern = "doc_list")) # check for these two patterns
sum(str_detect(file.list, pattern = "_doclist")) # 
file.list = str_subset(file.list, pattern = "_doclist", negate = T) # remove those with "_doclist" 
file.list = str_subset(file.list, pattern = "doc_list", negate = T) # but sometimes it can be this too; remove both
sum(str_detect(file.list, pattern = "doc_list")) # check if still have
sum(str_detect(file.list, pattern = "_doclist")) 


## folder number ====
file.list[1]
str_view(file.list, "^\\d{1,2}") # digit class from 1 to 2 digits
foldernum <- str_extract(file.list, "^\\d{1,2}")
summary(as.numeric(foldernum))
tabyl(as.numeric(foldernum))


## Let's make those full file paths that can be used to download the files ====
file.list
file.list2 = paste("source_data",file.list, sep="/") 
    # source_data because /source_data
    # change if the directory is different
    # paste source_data + file.list, separated by /
    # e.g., source_data + / + 1/Desire to serve draws more t....docx 

file.list2[1:2]

# SECTION B) EXTRACT DATA #### 

## trial with one document ====
trial = read_docx(file.list2[1])
trial
trial = read_docx(file.list2[3])
trial
##  text body ====
# see that the body of news starts at line after "Body: and end at line before "Classification"
start.text = which(trial == "Body") + 1
end.text   = which(trial == "Classification") - 1

text = paste(trial[start.text:end.text], collapse = "\n")
cat(text, "\n")

## Next, let's grab each of the options that has an explicit tag. ====

(section = gsub("Section:","",trial[grepl("Section:",trial,fixed=T)] ,fixed=T))
# grepl() will find in trial the pattern = "Section" and return a logical vector.
# If true, then trial[ ] will take out that entire string value
# gsub will remove remove from that string value "Section:" and replace with "" (blank)
(section = str_replace(trial[str_detect(trial, "Section:")], pattern = "Section:", replacement =""))
# Need to first detect because the trial is in a vector
(words = gsub("Length:","",trial[grepl("Length:",trial,fixed=T)] ,fixed=T))
(language = gsub("Language:","",trial[grepl("Language:",trial,fixed=T)],fixed=T))

(type = str_replace(trial[str_detect(trial, "Publication-Type:")], pattern = "Publication-Type:", replacement =""))
# trial[1] has a section on "Publication-Type:" 
# trial[3] does not have. So if just run codes withOUT ifelse, the type vector will have no data.
# This will become a problem in the loop later (the loop will stop because entering NO data into the dataframe)
# This ifelse code => if length is > 0, then ran the gsub. Else, input NA 
(type = ifelse(length(trial[grepl("Publication-Type:",trial,fixed=T)]) >0,
               gsub("Publication-Type:","",trial[grepl("Publication-Type:",trial,fixed=T)],fixed=T),
               NA))
(subject = gsub("Subject:","",trial[grepl("Subject:",trial,fixed=T)],fixed=T))
(industry = gsub("Industry:","",trial[grepl("Industry:",trial,fixed=T)],fixed=T))
(geographic = gsub("Geographic:","",trial[grepl("Geographic:",trial,fixed=T)],fixed=T))
# (load.date = gsub("Load-Date:","",trial[grepl("Load-Date:",trial,fixed=T)],fixed=T))

# These below are relational -> they should be the same position in every docu
(title = trial[1])
(source = trial[2])
(pub.date = trial[3])

# the abv codes with one doc show that we can do these to get info
# the below begins to write codes with loop to get same info from ALL docs



## Now that we know how to extract all the information we can kick it up a notch and write a loop. ====

### Step 1 here is to create the empty data frame. ====
news <- data.frame( title = rep(NA, length(file.list2)),
                       source = rep(NA, length(file.list2)),
                       pub.date = rep(NA, length(file.list2)),
                       section = rep(NA, length(file.list2)),
                       words = rep(NA, length(file.list2)),
                       language = rep(NA,length(file.list2)),
                       type = rep(NA,length(file.list2)),
                       subject = rep(NA,length(file.list2)),
                       industry = rep(NA,length(file.list2)),
                       geographic = rep(NA,length(file.list2)),
                       text = rep(NA,length(file.list2)),
                       stringsAsFactors = F
)


### Step 2 is to create the loop by copying down the code we know extracts what we want and has it input it into our data frame. ====
for(i in 1:length(file.list2)) {
    print(paste("Working on document", i, "of", length(file.list2)))
    temp.doc = read_docx(file.list2[i])
    
    news$title[i] = temp.doc[1]
    news$source[i] = temp.doc[2]
    news$pub.date[i] = temp.doc[3]
    
    #news$section[i] = gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[8]
    news$section[i] = ifelse(length(temp.doc[grepl("Section:",temp.doc,fixed=T)]) >0,
                                gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    news$words[i] = gsub("Length:","",temp.doc[grepl("Length:",temp.doc,fixed=T)] ,fixed=T)
    news$language[i] = gsub("Language:","",temp.doc[grepl("Language:",temp.doc,fixed=T)] ,fixed=T)
    
    # news$type[i] = gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[3]    
    news$type[i] = ifelse(length(temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)]) >0, 
                             gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T),
                             NA)
    
    #news$subject[i] = gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T)
    news$subject[i] = ifelse(length(temp.doc[grepl("Subject:",temp.doc,fixed=T)] >0),
                                gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    
    #news$industry[i] = gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T)
    # at least one article 4 does not have industry
    news$industry[i] = ifelse(length(temp.doc[grepl("Industry:",temp.doc,fixed=T)] >0),
                                 gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T),
                                 NA)
    
    # news$geographic[i] = gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T)
    news$geographic[i] = ifelse(length(temp.doc[grepl("Geographic:",temp.doc,fixed=T)] >0),
                                   gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T),
                                   NA)
    
    start.text = which(temp.doc == "Body") + 1
    end.text   = which(temp.doc == "Classification") - 1
    
    news$text[i] = paste(temp.doc[start.text:end.text], collapse = "\n")
}

## Add in the vars that denote folder number ====
news$foldernum <- foldernum
dplyr::glimpse(news)

news <- news %>% 
    relocate(foldernum, .before = title )

nrow(news) # 3300

# SECTION C) REMOVE ARTICLES THAT ARE DUPLICATES, OR NON-RELEVANT ####
# Non-relevant articles include those about PAS political party and People Association due to search string "pa" 

## Check for duplicates in titles ====
news$title_duplicates <- duplicated(news$title)
tabyl(news$title_duplicates)

View.duplicates <- 
    news %>% 
    select(title, text, pub.date, title_duplicates) %>% 
    filter(title_duplicates == T)
    
### View(View.duplicates) # these obs are duplicates but will not include the original ones ====

### remove duplicates (n=52) ==== 
nrow(news) # 3300
news <- 
    news %>% 
    filter(title_duplicates != T) %>% 
    select(-title_duplicates)

nrow(news) # 3248 (3300 - 52 = 3248)


## Remove articles related to PA People's Associations and PAS (Political party) ==== 

str_view_all(news$title, regex(pattern = "\\bpas\\b", ignore_case = T))
#\\b states the start and end of the word boundary 
str_view_all(news$title, regex(pattern = "\\bpa\\b", ignore_case = T), match = T)

sum(str_detect(news$title, regex(pattern = "\\bpas\\b", ignore_case = T))) # count of 833
sum(str_detect(news$title, regex(pattern = "\\bpa\\b", ignore_case = T))) # count of 330

news <- 
    news %>% 
    mutate(pas = str_detect(title,
                            regex(pattern = "\\bpas\\b", ignore_case = T))) %>% 
    mutate(pa = str_detect(title,
                           regex(pattern = "\\bpa\\b", ignore_case = T)))
tabyl(news$pas) # 833
tabyl(news$pa) # 330

news <- 
    news %>% 
    filter(pas != T & pa != T)

nrow(news) # 2085 because 3248 - 833 - 330

news <-
    news %>% 
    select(-pa, -pas)

# FINAL SECTION) save as Rdata file #####
getwd()
save(news, file = "cr_data/news.RData") 
load(file = "cr_data/news.RData")



