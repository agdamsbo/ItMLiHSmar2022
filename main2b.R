## ItMLiHSmar2022
## Script main2b.R is wrtitten to follow the same structure as main2b.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)

## Loading data
rm(list = ls())
dta<-read.csv("csfBiomarkers_CSVFILE.csv")

# Only use a single feature
idx = 126
X = dta[,idx]
y<-factor(dta$group)

## Levels of y
catinfo<-levels(y)

# Create random partition of data
set.seed(3)

K = 10

c<-createFolds(y=y, k = K, list = TRUE, returnTrain = FALSE)

accTrain<-accTest<-errTrain<-errTest<-c()
cMatTrain<-cMatTest<-table(factor(c(0,0),levels=catinfo),factor(c(0,0),levels=catinfo))

## Iterate over partitions
for (idx1 in 1:K){
  # idx1=2
  # Get training- and test sets
  I_test = 1:length(y) %in% c[[idx1]] ## Creating selection vector of TRUE/FALSE
  I_train = !I_test
  #
  Xtrain = X[I_train]
  ytrain = y[I_train]
  Xtest = X[I_test]
  ytest = y[I_test]
  
  # Train linear regression model
  fit<-glm(y~x,
           data=data.frame(x=Xtrain,y=ytrain),
           family = binomial)
  
  # Use model to predict
  yhatTrainProb = predict(fit,data.frame(x=Xtrain), type="response")
  yhatTestProb =  predict(fit,data.frame(x=Xtest), type="response")
  
  # Compute training and test error
  yhatTrain = round(yhatTrainProb)
  yhatTest = round(yhatTestProb)
  
  # Make predictions categorical again (instead of 0/1 coding)
  yhatTrainCat = factor(round(yhatTrainProb),levels=c("0","1"),labels=catinfo)
  yhatTestCat = factor(round(yhatTestProb),levels=c("0","1"),labels=catinfo)
  
  # Evaluate classifier performance
  # Accuracy
  accTrain[idx1] = sum(yhatTrainCat==ytrain)/length(ytrain)
  accTest[idx1] = sum(yhatTestCat==ytest)/length(ytest)
  #
  # Error rate
  errTrain[idx1] = 1 - accTrain[idx1]
  errTest[idx1] = 1 - accTest[idx1]
  
  # Compute confusion matrices
  cMatTrain = cMatTrain + table(ytrain,yhatTrainCat)
  cMatTest = cMatTest + table(ytest,yhatTestCat)
}

ptrain<-ggplot(data = as.data.frame(cMatTrain),
               mapping = aes(x = Var1,
                             y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +# if your results aren't quite as clear as the above example
  labs(title="Training data")+
  xlab("yhatTrainCat")+
  ylab("ytrain")

ptest<-ggplot(data = as.data.frame(cMatTest),
              mapping = aes(x = Var1,
                            y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +# if your results aren't quite as clear as the above example
  labs(title="Test data")+
  xlab("yhatTestCat")+
  ylab("ytest")
