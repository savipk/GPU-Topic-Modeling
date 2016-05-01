
library(tm)
library(topicmodels)

setwd("C:/UVa/own work/workspace/PNASjournals")

# assigning the directory of the files to an object
journals2014 <- getwd()
# checking the object
dir(journals2014)

# Creating a Corpus
Journal.corpus <- Corpus(DirSource(journals2014))
# Inspecting the Corpus
summary(Journal.corpus)
# The id of any document in the corpus
Journal.corpus[[3537]]$meta$id
# The content of a document in the corpus
Journal.corpus[[1]]$content

# I am using only first 100 documents in the corpus
Journal.corpus.samp <- Journal.corpus[1:2500]

# Applying topic modelling processing steps to the corpus to get it up for analysis
j2014.corp <- tm_map(Journal.corpus.samp, content_transformer(tolower))
# removing special characters
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, " ",x))
})
j2014.corp <- tm_map(j2014.corp, removespec, "[—†â‡’“ï€ˆ™œ]")
j2014.corp <- tm_map(j2014.corp, removespec, "€“")
j2014.corp <- tm_map(j2014.corp, removespec, "€™")
j2014.corp <- tm_map(j2014.corp, removeNumbers)
j2014.corp <- tm_map(j2014.corp, removePunctuation)
j2014.corp <- tm_map(j2014.corp, stripWhitespace)
j2014.corp <- tm_map(j2014.corp, removeWords, stopwords("english"))
j2014.corp <- tm_map(j2014.corp, stemDocument)

# checking the contents
j2014.corp[[1]]$content

# creating a document term matrix
pnas.dtm <- DocumentTermMatrix(j2014.corp)
pnas.dtm
# inspecting the term document matrix
inspect(pnas.dtm)
# This shows a 96% sparsity which needs to be reduced before trying our hand at Latent Dirichlet Allocation

# find the frequency
word.freq <- colSums(as.matrix(pnas.dtm))
length(word.freq)

# ordering the words in the terms of their freq.
ord <- order(word.freq)

# Removing the sparsity, that keeping the empty space to 50%.
pnas.dtm2 <- removeSparseTerms(pnas.dtm, 0.5)
inspect(pnas.dtm2)

# inspecting the document term matrix
pnas.dtm2
# we area able to reduce the sparsity to 32%, this helps us reduce the dimentionality of the matrix
# without affecting the word relationship

# reducing the empty space to 90%
pnas.dtm3 <- removeSparseTerms(pnas.dtm, 0.9)
pnas.dtm3
# the sparsity is reduced to 71%
inspect(pnas.dtm3)

# use ngrams for creating different sets for different types of predictions
#
#
#
#
#
#




#**********************************************************************************************
                              ########### Model 1 #############
#***********************************************************************************************

# Creating LDA model with VEM and aplha 50/k, I am using 4 topic which represent the
# four broad categories of Physical Sciences, Social Sciences, Biological Sciences
# and others
# the seed value is kept to maintain reproducibility.

pnas.lda.vem <- LDA(pnas.dtm3, 3, method = 'VEM', control = list(seed= 300))

terms(pnas.lda.vem, 30)
# after inspection of words we can say that
# Topic1: Physical Sciences
# Topic2: Biological Sciences
# Topic3: Social Sciences
pnas.lda.vem@gamma[1:10, ]

# use the prediction corpus, analysis performed in separate file "Pred corp for capst"

# prediction model
t <- posterior(pnas.lda.vem, pnas.dtm.5)
# used corpus
t1 <- posterior(pnas.lda.vem, pnas.dtm3)

##################################### prediction corpus ####################################


# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic <- data.frame(t(t$terms))
colnames(term.topic) <- c("Physical Sciences", "Biological Sciences", "Social Sciences")

terms.bio <- data.frame(term.topic$`Biological Sciences`)
rownames(terms.bio) <- rownames(term.topic)

terms.socialsc <- data.frame(term.topic$`Social Sciences`)
rownames(terms.socialsc) <- rownames(term.topic)

terms.phy <- data.frame(term.topic$`Physical Sciences`)
rownames(terms.phy) <- rownames(term.topic)

# I can perform word cloud visualization using these data frames and even compare the words
# this is done in another file, "Wordcloud_corpfor caps"

# I can find the word associations here and can form better models
#
#
#
#
#
#



# finding the most likely word in each topics.
term.topic$most.likely.topic <- colnames(term.topic)[apply(term.topic, 1, which.max)]

# creating the bag of words of each topic
terms.in.bio <- rownames(term.topic)[term.topic$most.likely.topic == "Biological Sciences"]

terms.in.phys <- rownames(term.topic)[term.topic$most.likely.topic == "Physical Sciences"]

terms.in.social <- rownames(term.topic)[term.topic$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic <- data.frame(t$topics)
colnames(docu.topic) <- c( "Physical Sciences", "Biological Sciences", "Social Sciences")

# finding the most likely topic for each document
docu.topic$most.likely.topic <- colnames(docu.topic)[apply(docu.topic, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio <- rownames(docu.topic)[docu.topic$most.likely.topic == "Biological Sciences"]

docu.in.phys <- rownames(docu.topic)[docu.topic$most.likely.topic == "Physical Sciences"]

docu.in.social <- rownames(docu.topic)[docu.topic$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#


##################################### Used model corpus ####################################













#**********************************************************************************************
                                ########### Model 2 #############
#***********************************************************************************************

# Creating LDA model with Gibbs Sampler and aplha 50/k, I am using 4 topic which represent the
# four broad categories of Physical Sciences, Social Sciences, Biological Sciences
# and others
# the seed value is kept to maintain reproducibility.

pnas.lda.gibbs <- LDA(pnas.dtm3, 3, method = 'Gibbs', control = list(seed= 300))

terms(pnas.lda.gibbs, 30)
# after inspection of words we can say that
# Topic1: Biological Sciences
# Topic2: Social Sciences
# Topic3: Physical Sciences
pnas.lda.gibbs@gamma[1:10, ]


# using the prediction corpus for testing
t.gibbs <- posterior(pnas.lda.gibbs, pnas.dtm.5)

# using the model corpus
t1.gibbs <- posterior(pnas.lda.gibbs, pnas.dtm3)

##################################### prediction corpus ####################################


# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic2 <- data.frame(t(t.gibbs$terms))
colnames(term.topic2) <- c("Biological Sciences", "Social Sciences", "Physical Sciences")

terms.bio.gibbs <- data.frame(term.topic2$`Biological Sciences`)
rownames(terms.bio.gibbs) <- rownames(term.topic2)

terms.socialsc.gibbs <- data.frame(term.topic2$`Social Sciences`)
rownames(terms.socialsc.gibbs) <- rownames(term.topic2)

terms.phy.gibbs <- data.frame(term.topic2$`Physical Sciences`)
rownames(terms.phy.gibbs) <- rownames(term.topic2)

# I can perform word cloud visualization using these data frames and even compare the words
# this is done in another file, "Wordcloud_corpfor caps"

# I can find the word associations here and can form better models
#
#
#
#
#
#



# finding the most likely word in each topics.
term.topic2$most.likely.topic <- colnames(term.topic2)[apply(term.topic2, 1, which.max)]

# creating the bag of words of each topic
terms.in.bio.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Biological Sciences"]

terms.in.phys.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Physical Sciences"]

terms.in.social.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic2 <- data.frame(t.gibbs$topics)
colnames(docu.topic2) <- c( "Biological Sciences", "Social Sciences", "Physical Sciences")

# finding the most likely topic for each document
docu.topic2$most.likely.topic <- colnames(docu.topic2)[apply(docu.topic2, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Biological Sciences"]

docu.in.phys.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Physical Sciences"]

docu.in.social.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#


##################################### Used model corpus ####################################




















#-----------------------------------------------------------------------------------------------------

#######>>>>>>Comparing the words in model1 and model2, check the word cloud file<<<<<<###########
#__________________________________________________________________________________________________

# words for 'Biological Sciences'
comp.bio <- data.frame(matrix(ncol = 2, nrow = 1403))
colnames(comp.bio) <- c("Model1.words", "Model2.words")
comp.bio$Model1.words <- term.topic$`Biological Sciences`
comp.bio$Model2.words <- term.topic2$`Biological Sciences`
rownames(comp.bio) <- rownames(terms.bio)

# words for 'physical Sciences'
comp.phy <- data.frame(matrix(ncol = 2, nrow = 1403))
colnames(comp.phy) <- c("Model1.words", "Model2.words")
comp.phy$Model1.words <- term.topic$`Physical Sciences`
comp.phy$Model2.words <- term.topic2$`Physical Sciences`
rownames(comp.phy) <- rownames(terms.phy)


# words for 'Social Sciences'
comp.socialsc <- data.frame(matrix(ncol = 2, nrow = 1403))
colnames(comp.socialsc) <- c("Model1.words", "Model2.words")
comp.socialsc$Model1.words <- term.topic$`Social Sciences`
comp.socialsc$Model2.words <- term.topic2$`Social Sciences`
rownames(comp.socialsc) <- rownames(terms.socialsc)




#-------------------------------------------------------------------------------------------------
setwd("C:/UVa/own work/workspace")
save.image("Capstonebechmarking.RData")
setwd("C:/UVa/own work/workspace/PNASjournals")
