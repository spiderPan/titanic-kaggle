library(tidyverse)
library(GGally)
library(gridExtra)

training<-read_csv('train.csv')
testing<-read_csv(("test.csv"))

full<-bind_rows(training,testing)
rm(training,testing)

summary(full)
head(full)
str(full)

full$Survived<-as.factor(full$Survived)
full$Pclass<-as.factor(full$Pclass)
full$Embarked<-as.factor(full$Embarked)
training<-full[1:891,]

colnames(training)

training%>%select(c("Pclass","Sex","Age","Embarked","Fare","Survived"))%>%ggpairs(aes(col=Survived,alpha=.6))


a<-training%>%ggplot(aes(Pclass,fill=Survived))+geom_bar(position = "dodge")
b<-training%>%ggplot(aes(Embarked,fill=Survived))+geom_bar(position = "dodge")


training%>%ggplot(aes(Age,fill=Survived))+geom_histogram(position = "dodge")+facet_grid(.~Sex)

training%>%filter(Age>50,Sex=='female')%>%select(c(Survived,Pclass,Sex,Age, SibSp, Parch, Fare , Cabin, Embarked))
training%>%filter(Age>60,Sex=='male')%>%select(c(Survived,Pclass,Sex,Age, SibSp, Parch, Fare , Cabin, Embarked))

training%>%ggplot(aes(Fare,fill=Survived))+geom_histogram(position = "dodge")+facet_grid(.~Pclass)

training%>%ggplot(aes(Pclass,fill=Survived))+geom_bar(position = "dodge")+facet_grid(.~Embarked)




grid.arrange(a,b,layout_matrix=matrix(c(1,2),ncol = 2))

table(full$Pclass,full$Embarked)
