

# creating the benchmark by finding the most common terms
library(wordcloud)

################# Model 1 #########################
pr1 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.bio, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Biological Sciences in the 1st model")
par(pr1)


pr2 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.phy, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Physial Sciences in the 1st model")
par(pr2)


pr3 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.socialsc, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Social Sciences in the 1st model")
par(pr3)


pr4 <- par(mar= c(0,0,4,0))
comparison.cloud(term.topic, max.words=100, 
                 scale=c(3, 1.5), random.order=FALSE, 
                 colors=c("blue3", "red3", "green3"), title.size=1)
text(0.5, 1, "Comparison of words between the 3 topics in the 1st model")
par(pr4)


################ Model 2 #######################

pr5 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.bio.gibbs, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Biological Sciences in the 2nd model")
par(pr5)


pr6 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.phy.gibbs, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Physical Sciences in the 2nd model")
par(pr6)


pr7 <- par(mar= c(0,0,4,0))
commonality.cloud(terms.socialsc.gibbs, max.words = 100,
                  scale= c(2.5, .50), random.order=F, col= rainbow(20))
text(0.5, 1, "WOrds in Social Sciences in the 2nd model")
par(pr7)


pr8 <- par(mar= c(0,0,4,0))
comparison.cloud(term.topic2, max.words=100, 
                 scale=c(3, 1.5), random.order=FALSE, 
                 colors=c("blue3", "red3", "green3"), title.size=1)
text(0.5, 1, "Camparison of Words in 3 topics in the 2nd model")
par(pr8)


############# Comparing Model1 and Model2 ###################

# comparison of words in the biological sciences
op1 <- par(mar= c(0,0,4,0))
comparison.cloud(comp.bio, max.words = 50,
                 scale = c(3, 1.5), random.order = F,
                 colors = c("blue3", "red3"), title.size = 1)
text(.5, 1, "Comparison between Words in Biological Sciences in two models")
par(op1)

# comparison of words in the social sciences
op2 <- par(mar= c(0,0,4,0))
comparison.cloud(comp.socialsc, max.words = 50,
                 scale = c(3, 1.5), random.order = F,
                 colors = c("blue3", "red3"), title.size = 1)
text(.5, 1, "Comparison between Words in Social Sciences in two models")
par(op2)


# comparison of words in the physial sciences
op <- par(mar= c(0,0,4,0))
comparison.cloud(comp.phy, max.words = 50,
                 scale = c(3, 1.5), random.order = F,
                 colors = c("blue3", "red3"), title.size = 1)
text(.5, 1, "Comparison between Words in Physical Sciences in two models")
par(op)

