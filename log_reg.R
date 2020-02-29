library(InformationValue)
library(caret)
library(aod)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

#to check the no.of null values
sum(is.na(mydata))

#dependent - admit
#predictor variables - gre,gpa,rank

windows()
plot(mydata$admit)

windows()
plot(mydata$rank)

# to check class bias
table(mydata$admit)
#this shows that the proportion of event occuring is smaller than that of event not occuring

summary(mydata)
# this shows that there are a greater no.of rejects than acceptence since mean of admiy os <0.5

sapply(mydata,sd)

#contigency table
xtabs(~admit+rank,data = mydata)


mydata$rank = factor(mydata$rank)

# to estimate the logistic regression
logit = glm(admit~gre+gpa+rank,data = mydata,family = "binomial")
logit


# to predict the log(odds)
#plogis bounds the prediction between 0 and 1
predicted = predict(logit,mydata,type = "response")
predicted


#optimal prediction probability cutoff
optCutoff = optimalCutoff(mydata$admit,predicted)
optCutoff


# to find the beta-coeff, std error, z value and p value
summary(logit)

anova(logit)

#chi-sq test
wald.test(Sigma = vcov(logit),b=coef(logit),Terms = 4:6)
#the overall effect of rank is significant


#misclassification error - % mismatch of predicted vs actual
misClassError(mydata$admit,predicted,threshold = optCutoff)


#Receiver Operating Characteristics Curve - %of true +ve accurately predicted
windows()
plotROC(mydata$admit,predicted)
#for a good model it should rise steeply


# to measure the quality of the model
Concordance(mydata$admit,predicted)


plot(logit)


#PREDICTION
# Given gre = 750 , gpa = 3.1, rank = 3
x = data.frame(gre = 800,gpa = 3.6,rank = as.factor(2))
predict(logit,x)
# only 4% chance that he gets admitted to clg
