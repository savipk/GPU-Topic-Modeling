#
# Capstone sample code.
#
# the below code is refferred from prof. Michele course 'text as data' and then modified as per use.
# (this is just a sample code, we will change the corpus and the data afterwards)
library(rvest)
library(magrittr)
library(koRpus)
library(tm)
library(topicmodels)

# creating a directory
if (!file.exists("corpus")) {
  dir.create("corpus")
}

setwd("corpus")
# the source page of the url from where the data is to be scraped
source.page <- read_html("http://www.presidency.ucsb.edu/sou.php")
#
# Scraping the data using rvest and selector gadget
# scraping the url's of the speechs
url1 <- source.page %>%
  html_nodes(".ver12:nth-child(3) a") %>%
  html_attr("href")
url2 <- source.page %>%
  html_nodes(".ver12:nth-child(4) a") %>%
  html_attr("href")
url3 <- source.page %>%
  html_nodes(".ver12:nth-child(5) a") %>%
  html_attr("href")
url4 <- source.page %>%
  html_nodes(".ver12:nth-child(6) a") %>%
  html_attr("href")
url5 <- source.page %>%
  html_nodes(".ver12:nth-child(7) a") %>%
  html_attr("href")
url <- c(url1, url2, url3, url4, url5)
#
# Scraping the name of the url's(year)
name1 <- source.page %>%
  html_nodes(".ver12:nth-child(3) a") %>%
  html_text()
name2 <- source.page %>%
  html_nodes(".ver12:nth-child(4) a") %>%
  html_text()
name3 <- source.page %>%
  html_nodes(".ver12:nth-child(5) a") %>%
  html_text()
name4 <- source.page %>%
  html_nodes(".ver12:nth-child(6) a") %>%
  html_text()
name5 <- source.page %>%
  html_nodes(".ver12:nth-child(7) a") %>%
  html_text()
linkname <- c(name1, name2, name3, name4, name5)
#
# creating the dataframe
df <- data.frame(url = url, link= linkname, stringsAsFactors=FALSE)
# taking only the speeches from 1934 to 2015
speech <- subset(x=df, link %in% 1934:2015)

# scraping the speeches
for(i in 1: nrow(speech)){
  text <- read_html(speech$url[i]) %>%
    html_nodes(".displaytext , p") %>%
    html_text()
  filename <- paste0("sotu-", speech$link[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text)  # write the file
  sink()
}

# creating a corpus
sotu.corpus <- Corpus(DirSource(getwd()))
summary(sotu.corpus)


# Remove capitalization, punctuation, numbers, stopwords, white space; stem
sotu <- tm_map(sotu.corpus,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
sotu <- tm_map(sotu, toSpace, "\\.") 
sotu <- tm_map(sotu, toSpace, "â") 
sotu <- tm_map(sotu, removeNumbers) 
sotu <- tm_map(sotu, removeWords, c(stopwords("english"), "will")) 
sotu <- tm_map(sou, removeWords, c(stopwords("english"), "nation", "can", "must")) # nation added after inspection
sotu <- tm_map(sotu, removePunctuation) 
sotu <- tm_map(sotu, stripWhitespace)
sotu <- tm_map(sotu, stemDocument)

sotu[[1]]$content
#
# creating a training and testing data set for LDA
sotu.train <- sotu[1:40]
sotu.test <- sotu[41:79]
#
# creating a document term matrix for the training dataset
sotu.dtm <- DocumentTermMatrix(sotu.train, control = list(weighting = weightTf))

# remove empty documents
removedoc <- apply(sotu.dtm, 1, sum)
sotu.dtm <- sotu.dtm[removedoc >0, ]

# train with 20 topics
topic.sotu <- LDA(sotu.dtm, 20)

# look at the top 10 words within the first 5 topics
terms(topic.sotu, 10)[,1:5]
# look at the top 5 words within every topic
terms(topic.sotu, 5)
# look at the top 10 words within every topic
terms(topic.sotu, 10)

# look at the top 10 topics within the first 5 documents
topics(topic.sotu, 10)[,1:5]

# most likely topic
sotu.most.likely.topic = topics(topic.sotu, 1)
sotu.topic.clusters = split(sotu.corpus, sotu.most.likely.topic)
sotu.topic.clusters[[2]][[1]][1]

# gamma contains the document-topic matrix
topic.sotu@gamma[1:5,]

save.image(file = "../Capsproject1.RData")
