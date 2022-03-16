## ItMLiHSmar2022
## Script main4a.R is wrtitten to follow the same structure as main4a.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk
## See https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/support-vector-machines.html

## Loading libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(glmnet)
library(tidyr)
library(e1071)
library(tidymodels)
library(kernlab)

library(furrr)
furr

## Loading data
rm(list = ls()) # Clear
dta<-read.csv("csfBiomarkers_CSVFILE.csv")
X<-dta[,-ncol(dta)]
y<-factor(dta[,ncol(dta)])
catinfo<-levels(y)
# y<-ifelse(y=="Control",-1,1)


## k-fold function from the ISL-book, independent of packages.
K<-10
set.seed(231)

c<-createFolds(y=y, k = K, list = FALSE, returnTrain = TRUE) # Using this function to ensure both levels represented in alle folds

# Setting budget Cost
Cost<-2^seq(-15, 40, 1)

B<-list()

accTrain<-accTest<-err_train<-err_test<-nSV<-matrix(nrow = K,ncol = length(Cost))

cMatTrain<-cMatTest<-table(factor(c(0,0),levels=catinfo),factor(c(0,0),levels=catinfo))

svm_linear_spec <- svm_poly(degree = 1) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

## Iterate over partitions
for (idx1 in 1:K){
  
  # Status
  cat('Processing fold', idx1, 'of', K,'\n')
  
  idx1=1
  # Get training- and test sets
  I_train = c==idx1 ## Creating selection vector of TRUE/FALSE
  I_test = !I_train
  
  Xtrain = X[I_train,]
  ytrain = y[I_train]
  Xtest = X[I_test,]
  ytest = y[I_test]
  
  ## Standardising x
  # XtrainScale<-scale(Xtrain, center = TRUE, scale = TRUE)
  # XtestScale<-scale(Xtest, center = TRUE, scale = TRUE)
  
  # Matrix for keeping coef.s
  mat<-c()
  # Iterate over regularization strengths to compute training- and test
  # errors for individual regularization strengths.
  for (idx2 in 1:length(Cost)){
    idx2=1
    
    svm_linear_fit <- svm_linear_spec %>% 
      set_args(cost = Cost[idx2]) %>%
      fit(ytrain ~ ., data = data.frame(ytrain,Xtrain))
    
    mod<-svm(ytrain ~ ., 
             data = data.frame(ytrain,Xtrain),
             kernel = "linear", 
             cost = Cost[idx2],
             scale=TRUE,
             probability=TRUE)
    
    # summary(mod)
    
    beta<-coef(mod) #
    
    # Keep coefficients for plot
    mat <- cbind(mat,beta)
    colnames(mat)[idx2]<-paste0("b",idx2) ## Naming for easier plotting
    
    nSV[idx1,idx2] <- sum(mod$tot.nSV)
    
    summary(mod)
    
    # Predict
    yhatTrainProb<-predict(mod,
                           newdata = Xtrain
    )
    
    yhatTestProb<-predict(mod,
                          newdata = Xtest
    )
    
    # Make predictions categorical again (instead of -1/1 coding)
    # yhatTrainCat = factor(ifelse(yhatTrainProb>0,"Impaired","Control"))
    # yhatTestCat = factor(ifelse(yhatTestProb>0,"Impaired","Control"))
    # yhatTestCat = factor(round(yhatTestProb),levels=c("0","1"),labels=catinfo)
    
    yhatTrainCat =  yhatTrainProb
    yhatTestCat = yhatTestProb
    
    # Evaluate classifier performance
    # Accuracy
    accTrain[idx1,idx2] = sum(yhatTrainCat==factor(ytrain,labels=catinfo))/length(ytrain)
    accTest [idx1,idx2] = sum(yhatTestCat==factor(ytest,labels=catinfo))/length(ytest)
    #
    # Error rate
    err_train[idx1,idx2] =  1 - accTrain[idx1,idx2]
    err_test [idx1,idx2] =   1 - accTest[idx1,idx2]
    
    # Compute confusion matrices
    cMatTrain = cMatTrain + table(factor(ytrain,labels=catinfo),yhatTrainCat)
    cMatTest = cMatTest + table(factor(ytest,labels=catinfo),yhatTestCat)
  }
  B[[idx1]]<-mat
}

# more elegant collapsing solution with apply()
errTrainPlot<-apply(err_train, 2, mean)
errTestPlot<-apply(err_test, 2, mean)
llog<-log2(Cost)

p20f<-ggplot()+
  geom_line(aes(x=rev(llog),y=errTrainPlot, color="blue"))+
  geom_line(aes(x=rev(llog),y=errTestPlot, color="green"))+
  # scale_x_continuous(trans='log10')+
  scale_color_discrete(labels = c("train","test"))+
  labs(title = "Classification by C",
       subtitle = "Checkpoint 20",
       color = "Model")+
  ylab("MSE")+
  xlab(bquote(log[2](C)))

# Number of support vectors
nSVPlot<-apply(nSV, 2, median)
p20i<-ggplot(aes(x=llog,y=nSVPlot, color="blue"), data=data.frame(llog,nSVPlot))+
  geom_line(show.legend = FALSE)+
  # geom_line(aes(x=llog,y=errTestPlot, color="green"))+
  # scale_x_continuous(trans='log10')+
  # scale_color_discrete(labels = c("train"))+
  labs(title = "Classification by C",
       subtitle = "Checkpoint 20",
       color = "Model")+
  ylab("number of support vectors")+
  xlab(bquote(log[2](C)))

# Beta array manipulation
dim<-dim(as.matrix(B[[1]]))

## Cleaning the matrix
## Necessary due to the lasso penalty
Bmatrix<-matrix(unlist(B),nrow=(dim[1])*(dim[2]))

Bmean<-matrix(
  apply(matrix(unlist(B),nrow=length(B[[1]])),1,mean, na.rm = TRUE), ## Rowwise mean calculation
  ncol = length(C))

## Saved beta values in list is "liquefied" and merged to a matrix
## Rowwise medians are calculated and the vector is returned as matrix with original dimensions.
Bmedian<-data.frame(coef.l=matrix(
  apply(Bmatrix,1,median, na.rm = TRUE),
  ncol = length(Cost)),
  features=c("int",colnames(X)))[-1,] # [-1,] to remove the intercept 

## Reducing even further for selection of best coefficients
BmedianSingle<-data.frame(features=Bmedian$features,
                          coef=apply(as.matrix(select(Bmedian,-features)),1,median, na.rm = TRUE))  

## Pivoting for plotting

BmedianLong <- Bmedian %>% pivot_longer(.,cols=-features) 

## Adding lambda values for plotting
BmedianLong$llog2<-rep(log2(Cost), length.out=nrow(BmedianLong)) 

# Plotting coefficients
p20h<-BmedianLong %>%
  ggplot(aes(x=rev(llog2),y=value,color=features))+ ## The x-scale is reversed to look like the matlab plot
  geom_point()+
  geom_line()+
  # scale_x_continuous(trans='log2') +
  # scale_color_discrete(labels = colnames(X))+
  labs(title = "Cross-validated model for k=10",
       subtitle = "Checkpoint 20",
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