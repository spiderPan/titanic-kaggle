library(tidyverse)
library(caret)
library(corrplot)
library(stringr)
library(randomForest)

training <- read_csv('train.csv')
testing <- read_csv('test.csv')

str(training)
str(testing)


full <- bind_rows(training, testing)
summary(full)
head(full)



(NameSet = str_split_fixed(full$Name, ',', 2))
(FirstNameSet = str_split_fixed(str_trim(NameSet[, 2]), "[.]",2))

full$LastName = NameSet[, 1]
(full$Title = str_trim(FirstNameSet[,1]))
(full$FirstName = str_trim(FirstNameSet[,2]))

str(full)

full$Family = full$SibSp + full$Parch + 1
full$IsFamily = paste(full$Family, full$LastName)
list <-
  c('Sex',
    'Ticket',
    'Survived',
    'Embarked',
    'Pclass',
    'Cabin',
    'IsFamily')

full[, list] <- lapply(full[, list], function(x) {
  y <- as.factor(x)
  y <- as.numeric(y)
  y
})

sum(is.na(full$Title))

full$Title %>% table()

full$Survived<-as.factor(full$Survived)
full$Title<-as.factor(full$Title)

full[1:nrow(training),] %>% ggplot(aes(Survived, fill=Survived))+geom_bar()+facet_grid(.~Title)

full$Title %>% table()
full[1:nrow(training),] %>% filter(Title=='Col') %>% nrow()

full1 <- full

full$Title <- as.character(full$Title)

full1$Title[full1$Title %in% c("Capt",'Col','Don','Dona','Jonkheer','Lady','Major','Mlle','Mme',
                             'Ms','Rev','Dr','Sir','the Countess')]<-"Rare"

str(full1)

summary(full1)

full[is.na(full1$Embarked),] %>% str()

full[is.na(full1$Fare),] %>% str()

full1 %>% filter(Cabin==29) %>% str()

full1 %>% select(Ticket,Embarked) %>% table()
full1 %>% select(Ticket,Embarked) %>% unique() %>% 
  ggplot(aes(Ticket,Embarked))+geom_col()

full1 %>% select(Ticket,Embarked) %>% unique() %>% 
  ggplot(aes(Ticket,Embarked))+geom_point()


inTrain = createDataPartition(training$Survived, p = 3 / 4)[[1]]
training_set = training[inTrain,]
testing_set = training[-inTrain,]

numeric_cols = na.omit(full[, list])
corrplot(cor(numeric_cols), method = "circle")




randomForest(Survived~.,data=training_set)


