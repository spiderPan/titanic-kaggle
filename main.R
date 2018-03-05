library(tidyverse)
library(caret)
library(corrplot)

training <- read_csv('train.csv')
testing <- read_csv('test.csv')

full <- bind_rows(training, testing)
summary(full)
head(full)

list <-
  c('Sex', 'Ticket', 'Survived', 'Embarked', 'Pclass', 'Cabin')

full[, list] <- lapply(full[, list], function(x) {
  y <- as.factor(x)
  y <- as.numeric(y)
  y
})

str(full)

inTrain = createDataPartition(training$Survived, p = 3 / 4)[[1]]
training_set = training[inTrain,]
testing_set = training[-inTrain,]

numeric_cols = na.omit(full[,list])
corrplot(cor(numeric_cols), method = "circle")
