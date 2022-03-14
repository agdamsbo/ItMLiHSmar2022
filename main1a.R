## ItMLiHSmar2022
## Script main1a.R is wrtitten to follow the same structure as main1a.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(R.matlab)

list2env(readMat("bodyMeasurementsSingleTrainTest.mat"),.GlobalEnv)

# Create an x axis with high resolution for plotting the trained model
XHighRes <- seq(from=min(c(Xtest,Xtrain)),to=max(c(Xtest,Xtrain)),length.out=1000)


# Define polynomial order
k = 1

source("polyExpand.R")

# Create polynomial expansion of input variables
XtrainPol = polyExpand(Xtrain,k)
XtestPol = polyExpand(Xtest,k)

XhighResPol = polyExpand(XHighRes,k)


## Scaling

scl = sd(XtrainPol)
XtrainPol = XtrainPol/scl
XtestPol = XtestPol/scl
XhighResPol = XhighResPol/scl


# Train linear regression model
fit<-lm(y~.,data=data.frame(x=XtrainPol,y=ytrain))
## Data frame is defined to secure consistent naming as moving forward

# Use model to predict
yhatTrain = predict(fit,data.frame(x=XtrainPol))
yhatTest = predict(fit,data.frame(x=XtestPol))

# Compute training and test error
errTrain = mean((ytrain-yhatTrain)^2)
errTest = mean((ytest-yhatTest)^2)

## Fit points
library(ggplot2)
# Plot the data, and the model predictions
p4f<-ggplot() + 
  geom_point(aes(x=XtrainPol, y=ytrain, color="red")) +
  geom_line(aes(x=XhighResPol, y=predict(fit,data.frame(x=XhighResPol)), color="blue")) +
  geom_point(aes(x=XtestPol, y=ytest, color="green")) +
  xlab("Chest circumference")+
  ylab("density D")+
  scale_color_discrete(labels = c("fit line","test","train"))

## MSE plot
p4g<-ggplot()+
  geom_line(aes(x=1:7,y=errTrain, color="blue"))+
  geom_line(aes(x=1:7,y=errTest, color="green"))+
  scale_y_continuous(trans='log10')+
  scale_color_discrete(labels = c("train","test"))+
  labs(color = "Model") +
  ylab("MSE")+
  xlab("polynomial order")
