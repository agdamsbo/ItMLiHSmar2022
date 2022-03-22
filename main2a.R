## ItMLiHSmar2022
## Script main2a.R is wrtitten to follow the same structure as main2a.m
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

# Make high resolution x axis for ploting
xHighRes = seq(0.5*min(X), 1.5*max(X),length.out=1000)

# Divide into training and test sets
set.seed(11)

# Hold out training partition
c<-createDataPartition(
  y,
  times = 1,
  p = 0.1,
  list = FALSE
)

# Partitioning
I_test = 1:length(y) %in% c ## Creating selection vector of TRUE/FALSE
I_train = !I_test
#
Xtrain = X[I_train]
ytrain = y[I_train]
Xtest = X[I_test]
ytest = y[I_test]

## Levels of y
catinfo<-levels(y)

# Train linear regression model
fit<-glm(y~x,
         data=data.frame(x=Xtrain,y=ytrain),
         family = binomial)

# Use model to predict
yhatTrainProb = predict(fit,data.frame(x=Xtrain), type="response")
yhatTestProb =  predict(fit,newdata=data.frame(x=Xtest), type="response")

yhatTrain = factor(round(yhatTrainProb),levels = c("0","1"),labels=catinfo)
yhatTest = factor(round(yhatTestProb),levels = c("0","1"),labels=catinfo)

## Plot
p11c<-ggplot() +
  # xlim(-6, 6) + 
  geom_line(aes(x=xHighRes , y=predict(fit,data.frame(x=xHighRes), type="response") , colour = "blue")) + # Colors are completely mixed??
  # geom_point(aes(x=,y=,colour = "red")) +
  labs(title = "Classification by tau", 
       subtitle = "Checkpoint 11",
       color = "Model")+
  xlab("tau")+
  ylab("propability")+
  scale_color_discrete(labels = "P from tau")

## Confusion matrices
trainTable<-table(yhatTrain, ytrain)
testTable<-table(yhatTest, ytest)

ptrain<- as.data.frame(trainTable) %>% 
  ggplot(mapping = aes(x = yhatTrain,
                       y = ytrain)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +# if your results aren't quite as clear as the above example
  labs(title="Training data")

ptest<- as.data.frame(testTable) %>% 
  ggplot(mapping = aes(x = yhatTest,
                       y = ytest)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +# if your results aren't quite as clear as the above example
  labs(title="Test data")

# library(caret)
# confusionMatrix(trainTable)
# confusionMatrix(testTable)