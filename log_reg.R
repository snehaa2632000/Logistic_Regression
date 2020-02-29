mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

#dependent - admit
#predictor variables - gre,gpa,rank

summary(mydata)
sapply(mydata,sd)

#contigency table
xtabs(~admit+rank,data = mydata)

mydata$rank = factor(mydata$rank)

# to estimate the logistic regression
logit = glm(admit~gre+gpa+rank,data = mydata,family = "binomial")
summary(logit)
