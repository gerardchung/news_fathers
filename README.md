# news_fathers
Text-analysis of news about fathers in Singapore

---
output:
  pdf_document: default
  html_document: default
---
# Text-analysis of news about fathers in Singapore :singapore:

**README WORK IN PROGRESS**

## Data-Search
I searched for news articles in __Nexis University__ database 

Search date was in June 2021 

A total of 3300 articles were downloaded in html format. 

**Search parameters**

 (1) Date of articles: All date range available 
 
 (2) Language: English
 
 (3) Date: Search was done by year 
 
 (4) Sources: Three news sources in Singapore:
 
  * [Channel NewsAsia](https://www.channelnewsasia.com/news/singapore)
        
  * [Today (Singapore)](https://www.todayonline.com/)
        
  * [The Straits Times (Singapore)](https://www.straitstimes.com/global)
        
 (5) Search string: "(father\*) OR (dad\*) OR (papa\*) OR (stepfather\*) OR (paternal\*)"
 
 (6) Search fields: “title” 
 
 (7) Geographical region: Singapore
 
**Results of search**: 3300 articles spanning year 1989-2021

***Note: Lexis University's policies do not allow me to share the data***

## Data-extraction
In Nexis Uni, the files are downloaded in docx files with each file containing 100 articles. 
Nexis Uni database displays 10 articles per web page and multiple clicking (10 clickings) are needed 
to collect up to 100 articles for each saving. The data extraction involves extracting text content and meta-data information (e.g. title, data, source, author etc) from
raw microsoft word documents. 

Extracting data (e.g., title, main body, source, geographic etc) from Lexis's docx files requires some coding using stringr commands based on patterns in each article. For codes on the extraction, see **cr_extraction.R** 


## Data-Preparations 

My data preparation involves removing duplicates, removing irrelevant articles (e.g., PA People's Association; PAS political party etc), and cleaning the meta-data variables (e.g., date, source of article, etc).
Codes for data preparations are in **cr_extraction.R** & **cr_prepdata.R** 

__Final Sample Size__: 2085

## Data-Preprocessing
WIP


## Task List ##
**Task List**

- [x] Data-collection
- [x] Training model
- [x] Cleaning data
- [x] Descriptives
- [ ] Analysis 

