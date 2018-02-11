library(caret)
library(readr)

training_data <- read.csv('data/train.csv')
train_set = createDataPartition(training_data$Survived,p=3/4,list=FALSE)
train = training_data[train_set,]
test = training_data[-train_set,]