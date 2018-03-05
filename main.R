library(tidyverse)
library(caret)
library(corrplot)
library(stringr)
library(randomForest)

training <- read_csv('train.csv')
testing <- read_csv('test.csv')

full <- bind_rows(training, testing)
summary(full)
head(full)

NameSet = str_split_fixed(full$Name, ',', 2)
FirstNameSet = str_split_fixed(str_trim(NameSet[, 2]), "[.]",2)

full$LastName = NameSet[, 1]
full$Title = str_trim(FirstNameSet[,1])
full$FirstName = str_trim(FirstNameSet[,2])

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
str(full)

inTrain = createDataPartition(training$Survived, p = 3 / 4)[[1]]
training_set = training[inTrain,]
testing_set = training[-inTrain,]

numeric_cols = na.omit(full[, list])
corrplot(cor(numeric_cols), method = "circle")



randomForest(Survived~.,data=training_set)


