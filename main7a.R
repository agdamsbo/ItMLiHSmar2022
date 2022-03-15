## ItMLiHSmar2022
## Script main7a.R is wrtitten to follow the same structure as main7a.m
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
library(ggdendro)

## Loading data
rm(list = ls()) # Clear
dta<-read.csv("csfBiomarkers_CSVFILE.csv")
X<-dta[,-131]
y<-factor(dta[,131])

Xscaled<-scale(X,scale=TRUE) ## This gives the same result as in the Matlab script
Xscaled_t<-t(Xscaled)

# barplot(apply(X, 2, sd), names.arg = 1:130,main="Barplot of feature SDs")

feat_8<-substr(colnames(X),1,8)
rownames(Xscaled_t)<-feat_8 ## To shorten axis labels


pbar<-tibble(sds=apply(X, 2, sd),
             features=feat_8)%>%
  ggplot(aes(x=features,y=sds))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Barplot of feature SDs")+
  ylab("Standard Deviation")+
  xlab("Features (First eight letters)")


ps<-Xscaled_t%>%
  dist()%>%
  hclust(method = "single")%>%
  ggdendrogram()+
  labs(title="Single")
pc<-Xscaled_t%>%
  dist()%>%
  hclust(method = "complete")%>%
  ggdendrogram()+
  labs(title="Complete")
pa<-Xscaled_t%>%
  dist()%>%
  hclust(method = "average")%>%
  ggdendrogram()+
  labs(title="Average")
