## ItMLiHSmar2022
## Script main1b.R is wrtitten to follow the same structure as main1b.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(R.matlab)

## Loading data

rm(list = ls())
list2env(mat<-readMat("bodyMeasurementsSingleCV.mat"),.GlobalEnv)
source("polyExpand.R")

## Run analysis

# Create random partition of data
set.seed(100)

K = 10

c<-createFolds(y=y, k = K, list = TRUE, returnTrain = FALSE)

errTrain<-errTest<-matrix(nrow = K,ncol = 7)
## Iterate over partitions
for (idx1 in 1:K){
  # idx1=2
  # Get training- and test sets
  I_train = 1:nrow(y) %in% c[[idx1]] ## Creating selection vector of TRUE/FALSE
  I_test = !I_train
  #
  Xtrain = X[I_train]
  ytrain = y[I_train]
  Xtest = X[I_test]
  ytest = y[I_test]
  
  # Iterate over polynomial orders
  for (k in 1:7){
    # k=2
    # Scale data
    XtrainPol = polyExpand(Xtrain,k)
    XtestPol = polyExpand(Xtest,k)
    
    scl = sd(XtrainPol)
    XtrainPol = XtrainPol/scl
    XtestPol = XtestPol/scl
    # XhighResPol = XhighResPol
    
    # Train linear regression model
    fit<-lm(y~poly(x,k),data=data.frame(x=Xtrain,y=ytrain))
    
    # Use model to predict
    yhatTrain = predict(fit,data.frame(x=Xtrain))
    yhatTest = predict(fit,data.frame(x=Xtest))
    
    # Compute training and test error
    errTrain[idx1,k] = mean((ytrain-yhatTrain)^2)
    errTest[idx1,k] = mean((ytest-yhatTest)^2)
  }
}

## Collapsing data
errTrainPlot<-errTestPlot<-c()
for (i in 1:7){
  errTrainPlot[i]<-mean(errTrain[,i])
  errTestPlot[i]<-mean(errTest[,i])
}

# Plot
p9d<-ggplot()+
  geom_line(aes(x=1:7,y=errTrainPlot, color="blue"))+
  geom_line(aes(x=1:7,y=errTestPlot, color="green"))+
  scale_y_continuous(trans='log10')+
  scale_color_discrete(labels = c("train","test"))+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 9",
       color = "Model")+
  ylab("MSE")+
  xlab("polynomial order")