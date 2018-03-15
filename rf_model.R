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

# Check data
head(full)
str(full)
summary(full)
sapply(full,function(x) sum(is.na(x)))

# Toolbox:

## Visualize Discrete Single Variables
vis_bar<-function(dataset,variable){
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
     ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_bar(position = "stack")+
     theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=8)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Continuous Single Variables
vis_hist<-function(dataset,variable,binwidths=5){
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
     ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
     theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable]],fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=8)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Discrete Multiple Variables
vis_bar_multi<-function(dataset,variable1,variable2){
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
     ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_bar(position = "stack")+
     theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=8)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Continuous Single Variables
vis_hist_multi<-function(dataset,variable1,variable2,binwidths=5){
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
     ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
     theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(dataset[[variable1]],fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+facet_grid(.~dataset[[variable2]])+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=8)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

# EDA - part 1 direct one variable
## Age
vis_hist(train,"Age",5)

## Sex
vis_bar(train,"Sex")

## Pclass
vis_bar(train,"Pclass")

# SibSp
vis_bar(train,"SibSp")

# Parch
vis_bar(train, "Parch")

# Embarked
vis_bar(train, "Embarked")

