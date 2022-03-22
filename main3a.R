## ItMLiHSmar2022
## Script main3a.R is wrtitten to follow the same structure as main3a.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(glmnet)
library(tidyr)
library(R.matlab)

## Loading data
rm(list = ls()) # Clear
list2env(readMat("bodyMeasurements.mat"),.GlobalEnv)
dta<-data.frame(X, y)%>%
  'colnames<-'(c(unlist(features),"y"))
colnames(X)<-unlist(features)

## k-fold function from the ISL-book, independent of packages.
K<-10
n<-nrow(X)
set.seed(25)

c<-sample(rep(1:K, length = n))  # This may give problems with classification

## Lambdas
lambdas<-2^seq(-10, 10, .5)

B<-list()
err_train<-err_test<-matrix(nrow = K,ncol = length(lambdas))
## Iterate over partitions
for (idx1 in 1:K){
  
  # Status
  cat('Processing fold', idx1, 'of', K,'\n')
  
  # idx1=2
  # Get training- and test sets
  I_train = c!=idx1 ## Creating selection vector of TRUE/FALSE
  I_test = !I_train
  
  Xtrain = X[I_train,]
  ytrain = y[I_train]
  Xtest = X[I_test,]
  ytest = y[I_test]
  
  ## Model matrices for glmnet
  # Xmat.train<-model.matrix(ytrain~Xtrain)  
  # Xmat.test<-model.matrix(ytest~Xtest)
  
  # Fit regularized linear regression model
  fit<-glmnet(Xtrain, ytrain, 
                    alpha = 0,        ## Alpha = 0 for ridge regression
                    lambda = lambdas, ## Setting lambdas
                    standardize = TRUE)
  
  beta<-coef(fit) #
  
  # Keep coefficients for plot
  B[[idx1]] = beta@x
  
  # Iterate over regularization strengths to compute training- and test
  # errors for individual regularization strengths.
  for (idx2 in 1:length(lambdas)){
    # idx2=2
    
    # Predict
    yhatTrain<-predict(fit, 
                       s = lambdas[idx2], 
                       newx=Xtrain
                       )
    
    yhatTest<-predict(fit, 
                      s = lambdas[idx2], 
                      newx = Xtest
                      )
    
    
    # Compute training and test error
    err_train[idx1,idx2] = mean((ytrain-yhatTrain)^2)
    err_test[idx1,idx2] = mean((ytest-yhatTest)^2)
  }
}

# more elegant collapsing solution with apply()
errTrainPlot<-apply(err_train, 2, mean)
errTestPlot<-apply(err_test, 2, mean)
llog<-log2(lambdas)

p14g<-ggplot()+
  geom_line(aes(x=llog,y=errTrainPlot, color="blue"))+
  geom_line(aes(x=llog,y=errTestPlot, color="green"))+
  # scale_x_continuous(trans='log10')+
  scale_color_discrete(labels = c("train","test"))+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 14",
       color = "Model")+
  ylab("MSE")+
  xlab(bquote(log[2](lambda)))

# Beta array manipulation
Bmean<-matrix(
  apply(matrix(unlist(B),nrow=length(B[[1]])),1,mean), ## Rowwise mean calculation
  ncol = length(lambdas))

## Saved beta values in list is "liquefied" and merged to a matrix
## Rowwise medians are calculated and the vector is returned as matrix with original dimensions.
Bmedian<-data.frame(coef.l=matrix(
  apply(matrix(unlist(B),nrow=length(B[[1]])),1,median), ## Rowwise median calculation
  ncol = length(lambdas)),
  features=c("int",unlist(features)))[-1,] # [-1,] to remove the intercept 

## Reducing even further for selection of best coefficients
BmedianSingle<-data.frame(features=Bmedian$features,
                          coef=apply(as.matrix(select(Bmedian,-features)),1,median))  

## Pivoting for plotting
BmedianLong <- Bmedian %>% pivot_longer(.,cols=-features) 

## Adding lambda values for plotting
BmedianLong$llog2<-rep(log2(lambdas), length.out=nrow(BmedianLong)) 

# Plotting coefficients
p14j<-BmedianLong %>%
  ggplot(aes(x=rev(llog2),y=value,color=features))+ ## The x-scale is reversed to look like the matlab plot
  geom_point()+
  geom_line()+
  # scale_x_continuous(trans='log2') +
  scale_color_discrete(labels = features)+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 14",
       color = "Model")+
  ylab("Coefficients")+
  xlab(bquote(log[2](lambda)))

### Native R
### This is the built-in function for n-fold cross-validation function with best lambda estimation.
### This only plots test error on the provided data set. From the ISL book

# set.seed(1) 
# cv.out<-cv.glmnet(Xtrain, ytrain, 
#                   # lambda=lambdas,
#                   nfolds = 10,
#                   alpha = 0)
# plot(cv.out)
# bestlam<-cv.out$lambda.min
