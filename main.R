library(tidyverse)
library(caret)

training<-read_csv('train.csv')
testing<-read_csv('test.csv')

full<-bind_rows(training,testing)
summary(full)
head(full)

inTrain = createDataPartition(training$Survived, p = 3/4)[[1]]
training_set = training[inTrain,]
testing_set = training[-inTrain,]

