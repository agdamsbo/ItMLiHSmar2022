## ItMLiHSmar2022
## Script main3b.R is wrtitten to follow the same structure as main3b.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(glmnet)
library(tidyr)

## Loading data
rm(list = ls()) # Clear
dta<-read.csv("csfBiomarkers_CSVFILE.csv")
X<-dta[,-131]
y<-factor(dta[,131])

## k-fold function from the ISL-book, independent of packages.
K<-10
n<-nrow(X)
set.seed(321)

# Using caret function to ensure both levels represented in all folds
c<-createFolds(y=y, k = K, list = FALSE, returnTrain = TRUE) 

# Lambdas
lambdas<-2^seq(-10, 5, 1)

B<-list()
accTrain<-accTest<-err_train<-err_test<-matrix(nrow = K,ncol = length(lambdas))

catinfo<-levels(y)

cMatTrain<-cMatTest<-table(factor(c(0,0),levels=catinfo),factor(c(0,0),levels=catinfo))


## Iterate over partitions
for (idx1 in 1:K){
  
  # Status
  cat('Processing fold', idx1, 'of', K,'\n')
  
  # idx1=1
  # Get training- and test sets
  I_train = c!=idx1 ## Creating selection vector of TRUE/FALSE
  I_test = !I_train
  
  Xtrain = X[I_train,]
  ytrain = y[I_train]
  Xtest = X[I_test,]
  ytest = y[I_test]
  
  ## Model matrices for glmnet
  Xmat.train<-model.matrix(~.-1,Xtrain)
  Xmat.test<-model.matrix(~.-1,Xtest)
  
  # Fit regularized linear regression model
  suppressWarnings(
  mod<-glmnet(Xmat.train, ytrain, 
              alpha = 1,        ## Alpha = 1 for lasso
              lambda = lambdas, ## Setting lambdas
              standardize = TRUE,
              family = "binomial")
  )
  
  beta<-coef(mod) #
  
  # Keep coefficients for plot
  B[[idx1]] <- as.matrix(beta)
  
  # Iterate over regularization strengths to compute training- and test
  # errors for individual regularization strengths.
  for (idx2 in 1:length(lambdas)){
    # idx2=1
    
    # Predict
    yhatTrainProb<-predict(mod, 
                           s = lambdas[idx2], 
                           newx = Xmat.train,
                           type = "response"
    )
    
    yhatTestProb<-predict(mod, 
                          s = lambdas[idx2], 
                          newx = Xmat.test,
                          type = "response"
    )
    
    # Compute training and test error
    yhatTrain = round(yhatTrainProb)
    yhatTest = round(yhatTestProb)
    
    # Make predictions categorical again (instead of 0/1 coding)
    yhatTrainCat = factor(round(yhatTrainProb),levels=c("0","1"),labels=catinfo)
    yhatTestCat = factor(round(yhatTestProb),levels=c("0","1"),labels=catinfo)
    
    # Evaluate classifier performance
    # Accuracy
    accTrain[idx1,idx2] <- sum(yhatTrainCat==ytrain)/length(ytrain)
    accTest [idx1,idx2] <- sum(yhatTestCat==ytest)/length(ytest)
    #
    # Error rate
    err_train[idx1,idx2] =  1 - accTrain[idx1,idx2]
    err_test [idx1,idx2] =   1 - accTest[idx1,idx2]
    
    # Compute confusion matrices
    cMatTrain = cMatTrain + table(ytrain,yhatTrainCat)
    cMatTest = cMatTest + table(ytest,yhatTestCat)
  }
}

# more elegant collapsing solution with apply()
errTrainPlot<-apply(err_train, 2, mean)
errTestPlot<-apply(err_test, 2, mean)
llog<-log2(lambdas)

p15f<-ggplot()+
  geom_line(aes(x=llog,y=errTrainPlot, color="blue"))+
  geom_line(aes(x=llog,y=errTestPlot, color="green"))+
  # scale_x_continuous(trans='log10')+
  scale_color_discrete(labels = c("train","test"))+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 15",
       color = "Model")+
  ylab("MSE")+
  xlab(bquote(log[2](lambda)))

# Beta array manipulation
dim<-dim(B[[1]])

## Cleaning the matrix
## Necessary due to the lasso penalty
Bmatrix<-matrix(unlist(B),nrow=(dim[1])*(dim[2]))
Bmatrix[Bmatrix==0]<-NA
   
Bmean<-matrix(
  apply(matrix(unlist(B),nrow=length(B[[1]])),1,mean, na.rm = TRUE), ## Rowwise mean calculation
  ncol = length(lambdas))

## Saved beta values in list is "liquefied" and merged to a matrix
## Rowwise medians are calculated and the vector is returned as matrix with original dimensions.
Bmedian<-data.frame(coef.l=matrix(
  apply(Bmatrix,1,median, na.rm = TRUE),
  ncol = length(lambdas)),
  features=c("int",colnames(X)))[-1,] # [-1,] to remove the intercept 

## Reducing even further for selection of best coefficients
BmedianSingle<-data.frame(features=Bmedian$features,
                          coef=apply(as.matrix(select(Bmedian,-features)),1,median, na.rm = TRUE))  

## Pivoting for plotting
Bmedian0<-Bmedian
Bmedian0[is.na(Bmedian0)]<-0 ## NAs are substituted with 0s for plotting
BmedianLong <- Bmedian0 %>% pivot_longer(.,cols=-features) 

## Adding lambda values for plotting
BmedianLong$llog2<-rep(log2(lambdas), length.out=nrow(BmedianLong)) 

# Plotting coefficients
p15i<-BmedianLong %>%
  ggplot(aes(x=rev(llog2),y=value,color=features))+ ## The x-scale is reversed to look like the matlab plot
  geom_point()+
  geom_line()+
  # scale_x_continuous(trans='log2') +
  # scale_color_discrete(labels = colnames(X))+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 15",
       color = "Model")+
  ylab("Coefficients")+
  xlab(bquote(log[2](lambda)))+
  theme(legend.position = "none")

## Creating confusion matrices
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
