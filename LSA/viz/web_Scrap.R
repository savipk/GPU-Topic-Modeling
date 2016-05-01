
# I am scraping the PNAS scientific journals for implementing LDA

# Step 1:
# I am creating a directory filled with PNAS Journal yearwise

library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

if (!file.exists("PNASjournals")) {
  dir.create("PNASjournals")
}
setwd("PNASjournals")

# The intial PNAS archives page
pagesource1 <- read_html("http://www.pnas.org/content/by/year")


# getting the links as per each year
url1 <- pagesource1 %>%
  html_nodes('.proxy-archive-year a') %>%
  html_attr("href")

year <- pagesource1 %>%
  html_nodes(".proxy-archive-year a") %>%
  html_text()

DF <- data.frame(url_links=url1, name_year= year)



# capturing the links to all journal for the year 2014 and 2015
DF1 <- data.frame(matrix(ncol=2))
nam <- c("url_link", "url_name")
names(DF1) <- nam
for(i in 5:6){
  pagesource2 <- read_html(paste0('http://www.pnas.org', DF$url_links[i]))
  url2 <- pagesource2 %>%
    html_nodes('.proxy-archive-by-year-month a') %>%
    html_attr('href')
  name_month <- pagesource2 %>%
    html_nodes('.proxy-archive-by-year-month a') %>%
    html_text()
  DF2 <- data.frame(url_link= url2, url_name=name_month)
  DF1 <- rbind(DF1, DF2)
}
DF1 <- DF1[-1, ]



# capturing the links to text for all the journal published in 2014
DF4 <- data.frame(matrix(ncol=2))
nam <- c("url_link", "url_name")
names(DF4) <- nam
for(i in 1:56){
  pagesource3 <- read_html(paste0('http://www.pnas.org', DF1$url_link[i]))
  url3 <- pagesource3 %>%
    html_nodes('.has-parent .first-item+ li a')%>%
    html_attr('href')
  journ_name <- pagesource3 %>%
    html_nodes('.has-parent .cit-title-group')%>%
    html_text()
  DF3 <- data.frame(url_link= url3, url_name= journ_name)
  DF4 <- rbind(DF4, DF3)
}

DF4 <- DF4[-1, ]


# capturing the links to text for all the journal published in 2014
DF6 <- data.frame(matrix(ncol=2))
nam <- c("url_link", "url_name")
names(DF6) <- nam
for(i in 57:nrow(DF1)){
  pagesource3 <- read_html(paste0('http://www.pnas.org', DF1$url_link[i]))
  url3 <- pagesource3 %>%
    html_nodes('.has-parent .first-item+ li a')%>%
    html_attr('href')
  journ_name <- pagesource3 %>%
    html_nodes('.has-parent .cit-title-group')%>%
    html_text()
  DF5 <- data.frame(url_link= url3, url_name= journ_name)
  DF6 <- rbind(DF6, DF5)
}

DF6 <- DF6[-1, ]


# scrapping the text from each journal for the year 2014
for(i in 1:762){
  pagesource4 <- read_html(paste0("http://www.pnas.org", DF4$url_link[i]))
  journ_text <- pagesource4 %>%
    html_nodes('#p-36 , #p-35 , #p-34 , #p-33 , #p-32 , #p-31 , #p-30 , #p-29 , #p-28 , #p-27 , #p-26 , #p-25 , #p-24 , #p-23 , #p-22 , #p-21 , #p-20 , #p-19 , #p-18 , #p-16 , #p-15 , #p-13 , #p-11 , #p-9 , #p-8 , #p-7 , #p-6 , #p-5')%>%
    html_text()
  name <- str_sub(DF4$url_link[i], start = 16, end = -6)
  # creating a file name
  filename <- paste0("2014-", name ,".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(journ_text)  # write the file
  sink() # close the file
  
}

# scrapping text from 764 onwards due to change in filename
for(i in 763:nrow(DF4)){
  pagesource4 <- read_html(paste0("http://www.pnas.org", DF4$url_link[i]))
  journ_text <- pagesource4 %>%
    html_nodes('#p-36 , #p-35 , #p-34 , #p-33 , #p-32 , #p-31 , #p-30 , #p-29 , #p-28 , #p-27 , #p-26 , #p-25 , #p-24 , #p-23 , #p-22 , #p-21 , #p-20 , #p-19 , #p-18 , #p-16 , #p-15 , #p-13 , #p-11 , #p-9 , #p-8 , #p-7 , #p-6 , #p-5')%>%
    html_text()
  name <- str_sub(DF4$url_link[i], start = 17, end = -6)
  # creating a file name
  filename <- paste0("2014-", name ,".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(journ_text)  # write the file
  sink() # close the file
}


# scrapping the text from each journal for the year 2015
dir.create("2015Journaltext")
setwd("2015Journaltext")

for(i in 1:648){
  pagesource4 <- read_html(paste0("http://www.pnas.org", DF6$url_link[i]))
  journ_text <- pagesource4 %>%
    html_nodes('#p-36 , #p-35 , #p-34 , #p-33 , #p-32 , #p-31 , #p-30 , #p-29 , #p-28 , #p-27 , #p-26 , #p-25 , #p-24 , #p-23 , #p-22 , #p-21 , #p-20 , #p-19 , #p-18 , #p-16 , #p-15 , #p-13 , #p-11 , #p-9 , #p-8 , #p-7 , #p-6 , #p-5')%>%
    html_text()
  name <- str_sub(DF6$url_link[i], start = 16, end = -6)
  # creating a file name
  filename <- paste0("2015-", name ,".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(journ_text)  # write the file
  sink() # close the file
}

for(i in 649:nrow(DF6)){
  pagesource4 <- read_html(paste0("http://www.pnas.org", DF6$url_link[i]))
  journ_text <- pagesource4 %>%
    html_nodes('#p-36 , #p-35 , #p-34 , #p-33 , #p-32 , #p-31 , #p-30 , #p-29 , #p-28 , #p-27 , #p-26 , #p-25 , #p-24 , #p-23 , #p-22 , #p-21 , #p-20 , #p-19 , #p-18 , #p-16 , #p-15 , #p-13 , #p-11 , #p-9 , #p-8 , #p-7 , #p-6 , #p-5')%>%
    html_text()
  name <- str_sub(DF6$url_link[i], start = 17, end = -6)
  # creating a file name
  filename <- paste0("2015-", name ,".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(journ_text)  # write the file
  sink() # close the file
}


save.image("CapstoneScrap.RData")
