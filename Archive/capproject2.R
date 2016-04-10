
# Capstone Code2
# try to predict the genre of the movie using a summary of the plot of the movie
# 
library(rvest)
library(plyr)
library(stringr)
library(RJSONIO)
library(magrittr)
library(koRpus)
library(tm)
library(topicmodels)

if(!file.exists("Moviecollection")){
  dir.create("MovieCollection")
}
setwd("MovieCollection")

year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

for( i in 1:length(year)){
  urlmov <- paste0("http://www.boxofficemojo.com/yearly/chart/?yr=", year[i], "&p=.html")
  movie_name <- read_html(urlmov)%>%
    html_nodes("td td b font a")%>%
    html_text()
  
  moviedata <- data.frame(movie_names= movie_name, yearofrelease = year[i])
  
  filename <- paste0(year[i], ".csv")
  sink(file = filename) %>% # open file to write
    cat(write.csv(moviedata))
  sink()
}

# combining all files together in one dataframe
files_data <- list.files(getwd(), pattern = ".csv")
moviesDF <- do.call(rbind, lapply(files_data, read.csv))

#performing the following operations on the movie name to make it readable for url
moviesDF$new_name <- str_replace_all(moviesDF$movie_names, "[\\?!]", "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "\\((.*?)\\)", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "both")
moviesDF$new_name <- str_replace_all(moviesDF$new_name, "2000$", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "right")
moviesDF$new_name <- str_replace_all(moviesDF$new_name,"[^a-zA-Z0-9\\-'.,]+" , "+")
moviesDF$new_name <- str_replace(moviesDF$new_name,"^The\\+" , "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "^Tyler\\+Perry's\\+", "")

# addressing some discrepancies in movie names
moviesDF$new_name[433] <- str_to_lower(moviesDF$new_name[433])
str_sub(moviesDF$new_name[991], start = 1, end = -1) <- "9+"
moviesDF$new_name[c(229,1076, 1023)] <- str_replace(moviesDF$new_name[c(229,1076, 1023)], "\\-", "")
moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131, 1364, 1370, 
                    1399)] <- str_replace(moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131,
                                                              1364, 1370, 1399)], "and\\+", "")
str_sub(moviesDF$new_name[c(34, 1201)], start = 1, end = 9) <- ""
str_sub(moviesDF$new_name[59], start = 8, end = -1) <- ""
str_sub(moviesDF$new_name[698], start = 9, end = -1) <- "+2"
str_sub(moviesDF$new_name[c(436,965)], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[157], start = 5, end = 6) <- "13"
str_sub(moviesDF$yearofrelease[188], start = 1, end = -1) <- "2000"
str_sub(moviesDF$yearofrelease[c(365,366)], start = 1, end = -1) <- "2002"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[c(510,930)], start = 1, end = -1) <- "2007"
str_sub(moviesDF$yearofrelease[668], start = 1, end = -1) <- "2005"
str_sub(moviesDF$yearofrelease[1096], start = 1, end = -1) <- "2009"
str_sub(moviesDF$yearofrelease[1164], start = 1, end = -1) <- "2010"
str_sub(moviesDF$yearofrelease[c(1265, 1273)], start = 1, end = -1) <- "2011"
moviesDF$new_name[231] <- str_replace(moviesDF$new_name[231], "The\\+", "")
str_sub(moviesDF$new_name[306], start = 1, end = -1) <- "X-Men+2"
str_sub(moviesDF$new_name[319], start = 10, end = 11) <- "3-D"
str_sub(moviesDF$new_name[570], start = -2, end = -1) <- "3-D"
moviesDF$new_name[366]<- str_c(moviesDF$new_name[366], "...", sep= "")
moviesDF$new_name[433]<- str_c("AVP+", moviesDF$new_name[433], sep= "")
moviesDF$new_name[570]<- str_c("The+", moviesDF$new_name[570], sep= "")
str_sub(moviesDF$new_name[510], start = 4, end = 4) <- "+and+"
str_sub(moviesDF$new_name[1045], start = -4, end = -4) <- "+and+"
str_sub(moviesDF$new_name[570], start = -6, end = -4) <- ""
str_sub(moviesDF$new_name[634], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[c(670,1256)], start = -3, end = -1) <- ""
str_sub(moviesDF$new_name[848], start = -5, end = -1) <- ""
str_sub(moviesDF$new_name[c(810,1211)], start = 1, end = 11) <- ""
str_sub(moviesDF$new_name[946], start = 1, end = -11) <- ""
str_sub(moviesDF$new_name[748], start = 2, end = 2) <- "%3A"
str_sub(moviesDF$new_name[961], start = -4, end = -1) <- ""
str_sub(moviesDF$new_name[1297], start = 8, end = 8) <- "!+"
# removing some invalid movie names
moviesDF <- moviesDF[-c(61, 993, 551, 1133, 1278, 1282, 1294), ] # 1278(TV Episode), 1282(TV episode), 1294(TV episode)

for(i in 1:nrow(moviesDF)){
  movie <- fromJSON(paste0("http://www.omdbapi.com/?t=", moviesDF$new_name[i], "&y=", moviesDF$yearofrelease[i], "&plot=full&r=json"))
  moviesDF$Genre[i] <- movie[[6]]
  moviesDF$Plot[i] <- movie[[10]]
}
# removing the unwanted columns
drops <- c("new_name", "X")
moviesDF <- moviesDF[ ,!(names(moviesDF) %in% drops)]

# creating the training corpus
movplt.train <- Corpus(VectorSource(moviesDF$Plot[1:1000]))
summary(movplt.train)

# Remove capitalization, punctuation, numbers, stopwords, white space; stem
corpus.train <- tm_map(movplt.train,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
corpus.train <- tm_map(corpus.train, toSpace, "\\.") 
corpus.train <- tm_map(corpus.train, removeNumbers) 
corpus.train <- tm_map(corpus.train, removeWords, c(stopwords('english'),"will")) 
corpus.train <- tm_map(corpus.train, removePunctuation)
corpus.train <- tm_map(corpus.train, stripWhitespace)
corpus.train <- tm_map(corpus.train, stemDocument)

# creating a document term matrix for the training dataset
movie.dtm <- DocumentTermMatrix(corpus.train, control = list(weighting = weightTf))

# remove empty documents
removedoc <- apply(movie.dtm, 1, sum)
movie.dtm <- movie.dtm[removedoc >0, ]

# creating a LDA model
topic.lda <- LDA(movie.dtm, 5)

# looking at top 5 words in all topics
terms(topic.lda, 5)

# Creating a test corpus
movplt.test <- Corpus(VectorSource(moviesDF$Plot[1001:1493]))

# Remove capitalization, punctuation, numbers, stopwords, white space; stem
corpus.test <- tm_map(movplt.test,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
corpus.test <- tm_map(corpus.test, toSpace, "\\.") 
corpus.test <- tm_map(corpus.test, removeNumbers) 
corpus.test <- tm_map(corpus.test, removeWords, c(stopwords('english'),"will")) 
corpus.test <- tm_map(corpus.test, removePunctuation)
corpus.test <- tm_map(corpus.test, stripWhitespace)
corpus.test <- tm_map(corpus.test, stemDocument)

# creating a document term matrix for the training dataset
movie.dtm2 <- DocumentTermMatrix(corpus.test, control = list(weighting = weightTf))

# remove empty documents
removedoc <- apply(movie.dtm2, 1, sum)
movie.dtm2 <- movie.dtm2[removedoc >0, ]

# predicting the topic using the lda model
topic.test <- posterior(topic.lda, movie.dtm2)
topic.test2 <- posterior(topic.lda, movie.dtm2)$topics


head(topic.test2)

# provide names of the topic with top 3 words
colnames(topic.test2) <- apply(terms(topic.lda, 3), 2, paste, collapse = ",")
head(topic.test2)
