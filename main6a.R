## ItMLiHSmar2022
## Script main6a.R is wrtitten to follow the same structure as main6a.m
## Andreas Gammelgaard Damsbo, agdamsbo@clin.au.dk

## Loading libraries
library(ggplot2)
library(patchwork)
library(dplyr)
# library(caret)
# library(glmnet)
library(tidyr)
# library(e1071)
library(R.matlab)
library(corrplot)

## Loading data
rm(list = ls()) # Clear

list2env(readMat("bodyMeasurements.mat"),.GlobalEnv)
dta<-data.frame(X, y)%>%
  'colnames<-'(c(unlist(features),"y"))
colnames(X)<-unlist(features)

X<-scale(X,scale=FALSE) ## This gives the same result as in the Matlab script

feat_8<-substr(colnames(X),1,8)

# p28b<-corrplot(cor(X%>%'colnames<-'(c(feat_8))),        # Correlation matrix
#                method = "color", # Correlation plot method
#                type = "full",    # Correlation plot style (also "upper" and "lower")
#                diag = TRUE,      # If TRUE (default), adds the diagonal
#                tl.col = "black", # Labels color
#                bg = "white",     # Background color
#                title = "",       # Main title
#                col = NULL)       # Color palette

p28c<-tibble(sds=apply(X, 2, sd),
             features=substr(colnames(X),1,8))%>%
  ggplot(aes(x=features,y=sds))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Barplot of feature SDs")+
  ylab("Standard Deviation")+
  xlab("Features (First eight letters)")


pca<-prcomp(X, scale = TRUE)
biplot(pca, scale = 0)

