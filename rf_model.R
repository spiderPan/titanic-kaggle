# Load packages
library(tidyverse)
library(randomForest)
library(ggthemes)
library(corrplot)
library(caret)
library(gridExtra)

# Load datasets
train<-read_csv("data/train.csv")
test<-read_csv("data/test.csv")

full<-bind_rows(train,test)
rm(train,test)

# Check data
head(full)
str(full)
summary(full)
sapply(full,function(x) sum(is.na(x)))

# EDA
## Age

(age_g1<-full %>% filter(!is.na(Survived)) %>% 
    ggplot(aes(Age,fill=as.factor(Survived)))+geom_histogram(position = "stack")+
    theme_economist()+labs(fill="Survived"))

(age_g2<-full %>% filter(!is.na(Survived)) %>% 
    ggplot(aes(Age,fill=as.factor(Survived)))+geom_histogram(position = "fill")+
    theme_economist()+labs(fill="Survived"))

(age_g3<-full %>% filter(!is.na(Survived)) %>% 
    ggplot(aes(Age,fill=as.factor(Survived)))+geom_histogram(position = "dodge")+
    theme_economist()+labs(fill="Survived"))

(Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
  labs(x="",y="")+
  annotate('text', x = 0, y = 0, label = "Age vs Survived \n multi_views",size=10)+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))

grid.arrange(Title,age_g1,age_g2,age_g3,layout_matrix=t(matrix(c(1,2,
                                                                 3,4),nrow=2)))

## Sex


