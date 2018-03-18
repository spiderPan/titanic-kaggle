# Load packages
library(tidyverse)
library(randomForest)
library(ggthemes)
library(corrplot)
library(caret)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(e1071)
library(party)
library(fastAdaboost)
library(kernlab)
library(kknn)
library(naivebayes)
library(keras)
library(glmnet)
library(Matrix)
library(LiblineaR)
library(sparsediscrim)
library(relaxo)
library(gbm)

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
  
  dataset$variable<-dataset[[variable]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "stack")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Continuous Single Variables
vis_hist<-function(dataset,variable,binwidths=5){
  
  dataset$variable<-dataset[[variable]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Discrete Multiple Variables
vis_bar_multi<-function(dataset,variable1,variable2){
  
  dataset$variable1<-dataset[[variable1]]
  dataset$variable2<-dataset[[variable2]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "stack")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Continuous Single Variables
vis_hist_multi<-function(dataset,variable1,variable2,binwidths=5){
  
  dataset$variable1<-dataset[[variable1]]
  dataset$variable2<-dataset[[variable2]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

# EDA
##part 1 direct one variable
### Age
train %>% vis_hist("Age")

#### Sex
train %>% vis_bar("Sex")

### Pclass
train %>% vis_bar("Pclass")

### SibSp
train %>% vis_bar("SibSp")

### Parch
train %>% vis_bar("Parch")

### Embarked
train %>% vis_bar("Embarked")

## Part 2 direct multi variable
### Age vs Sex
train %>% vis_hist_multi("Age","Sex")

### Age vs Pclass
train %>% vis_hist_multi("Age","Pclass")

### Pclass vs Sex
train %>% vis_bar_multi("Pclass","Sex")

### Embarked vs Pclass
train %>% vis_bar_multi("Embarked","Pclass")

## part 3 Data Processing
### New Var: Title
name_orig<-full$Name
full <- full %>% separate(Name,sep=",",c('Surname','FirstName')) %>%
  separate(FirstName,sep="\\. ",c('Title','Firstname')) %>% 
  mutate(Title=trimws(Title))

detach("package:plyr", unload=TRUE) 

full %>% group_by(Title) %>% summarise(avg_age=mean(Age,na.rm=T),
                                       num_ppl=n(),survival_rate=mean(Survived,na.rm=T))

#### Reassign rare titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

#### Reassign mlle, ms, and mme, and rare
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royalty]     <- 'Royalty'
full$Title[full$Title %in% officer]     <- 'Officer'
rm(royalty,officer,train,test,name_orig)

full %>% filter(!is.na(Survived)) %>% vis_bar('Title')

### New Var: Family Sizes
full$Fsize <- full$SibSp+full$Parch+1
full[1:891,] %>%  vis_bar('Fsize')

full$FsizeD[full$Fsize == 1] <- 'Alone'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'Small'
full$FsizeD[full$Fsize > 4] <- 'Big'

full[1:891,] %>% vis_bar('FsizeD')

### Impute: Embarked
full$F_Ticket<-as.numeric(as.factor(full$Ticket))

full %>% filter(!Ticket %in% c('113572')) %>% select(F_Ticket,Embarked) %>%
  unique() %>% filter(F_Ticket<60 & F_Ticket>25) %>% 
  ggplot(aes(F_Ticket,Embarked))+
  geom_col()+geom_vline(aes(xintercept=43),col='firebrick',lty=2,size=2)

full[is.na(full$Embarked),]$Embarked<-'S'

### New Variable: Adj_Fare
full <- full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>%
  right_join(full,by='Ticket') %>% mutate(Adj_Fare=Fare/ppl_ticket)


full[is.na(full$Fare),] %>% head()

full %>% filter(Pclass==3) %>% select(Adj_Fare) %>% summary()

full %>% filter(Pclass==3) %>% ggplot(aes(Adj_Fare))+
  geom_density(size=.5,alpha=.5,fill='grey')+
  geom_vline(aes(xintercept=median(Adj_Fare,na.rm=T)),lty=2,lwd=1.5,col='firebrick')+
  theme_few()

full[is.na(full$Fare),]$Adj_Fare <- median(full[full$Pclass==3,]$Adj_Fare,na.rm=T)

full[1:891,] %>% vis_hist('Adj_Fare')
full[1:891,] %>% vis_hist_multi('Adj_Fare','Pclass')


### New Variable: Adj_Age
full %>% filter(is.na(Age)) %>% group_by(Title) %>% summarise(num_ppl=n())

full <- full %>% filter(!is.na(Age)) %>% group_by(Title) %>% 
  summarise(avg_age=median(Age)) %>% right_join(full,by='Title') %>% 
  mutate(Adj_Age=if_else(is.na(Age),avg_age,Age))

full[1:891,] %>% vis_hist('Adj_Age')

### New Variable: Child
full$Child[full$Adj_Age < 18] <- 'Child'
full$Child[full$Adj_Age >= 18] <- 'Adult'

full[1:891,] %>% vis_bar_multi('Child','Sex')
full[1:891,] %>% vis_bar_multi('Child','Pclass')


rm(vis_bar,vis_bar_multi,vis_hist,vis_hist_multi)

## Correlation
selected<-full %>% select(Survived,Title,Sex,Pclass,FsizeD,Adj_Fare,Embarked,Child)
selected[1:891,] %>% sapply(function(x) as.numeric(as.factor(x))) %>% cor() %>% corrplot.mixed(upper='ellipse')

selected[,!colnames(selected) %in% c("Adj_Fare")]<-selected[,!colnames(selected) %in% c("Adj_Fare")] %>% 
  lapply(function(x) as.factor(x))

# Modeling
train<-selected[1:891,]
test<-selected[892:1309,]

rm(full,selected)

# Cross Validation
inTrain <- createDataPartition(train$Survived,p=.7,list=F)
ctrain  <- train[inTrain,]
validation <- train[-inTrain,]
rm(inTrain)

# gbm
(gbm<-train(Survived~.,data=ctrain,
              trControl=trainControl(method="repeatedcv", number=5, repeats=5),
              method='gbm'))

gbm_pred <- predict(gbm,validation)

confusionMatrix(gbm_pred,validation$Survived)
rm(gbm)

# rrlda
(rrlda<-train(Survived~.,data=ctrain,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='rrlda'))

rrlda_pred <- predict(rrlda,validation)

confusionMatrix(rrlda_pred,validation$Survived)
rm(rrlda)

# da
(da<-train(Survived~.,data=ctrain,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='dda'))

da_pred <- predict(da,validation)

confusionMatrix(da_pred,validation$Survived)
rm(da)

# log
(log<-train(Survived~.,data=ctrain,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='regLogistic'))

log_pred <- predict(log,validation)

confusionMatrix(log_pred,validation$Survived)
rm(log)

# glm
(glm<-train(Survived~.,data=ctrain,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='glmnet'))

glm_pred <- predict(glm,validation)

confusionMatrix(glm_pred,validation$Survived)
rm(glm)


# keras
# (keras<-train(Survived~.,data=ctrain,
#             trControl=trainControl(method="repeatedcv", number=5, repeats=5),
#             method='mlpKerasDropout'))
# 
# keras_pred <- predict(keras,validation)
# 
# confusionMatrix(keras_pred,validation$Survived)
# rm(keras)

# knn
(knn<-train(Survived~.,data=ctrain,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='kknn'))

knn_pred <- predict(knn,validation)

confusionMatrix(knn_pred,validation$Survived)
rm(knn)

# Naive Bay
(nb<-train(Survived~.,data=ctrain,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='naive_bayes'))

nb_pred <- predict(nb,validation)

confusionMatrix(nb_pred,validation$Survived)
rm(nb)

# Gaussian Process
(gp<-train(Survived~.,data=ctrain,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='gaussprPoly'))

gp_pred <- predict(gp,validation)

confusionMatrix(gp_pred,validation$Survived)
rm(gp)

# svm
(svm<-train(Survived~.,data=ctrain,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinearWeights'))

svm_pred <- predict(svm,validation)

confusionMatrix(svm_pred,validation$Survived)
rm(svm)

# adaboost
(adaboost<-train(Survived~.,data=ctrain,
                 trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                 method='adaboost'))

ada_pred <- predict(adaboost,validation)

confusionMatrix(ada_pred,validation$Survived)
rm(adaboost)

# CForest
(cforest<-train(Survived~.,data=ctrain,
                #trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                method='cforest',
                controls = cforest_unbiased(ntree = 1000)))
cforest_pred <- predict(cforest,validation)
confusionMatrix(cforest_pred,validation$Survived)
rm(cforest)

# xgboost
trControl <- trainControl(method="repeatedcv", number=5, repeats=5);
xgbGrid <- expand.grid(nrounds=c(30),
                       max_depth=c(8),
                       eta=c(0.1),
                       colsample_bytree=c(0.5),
                       subsample=c(1),
                       gamma=c(0),
                       min_child_weight=c(3))

(model.xgb <- train(Survived~.,data=ctrain,trControl=trControl,method='xgbTree',
                    tuneGrid = xgbGrid))

xgb_pred <- predict(model.xgb,validation)
confusionMatrix(xgb_pred,validation$Survived)
rm(trControl,xgbGrid,model.xgb)

combine<-bind_cols(Survived=validation$Survived,
                   ada=ada_pred,
                   cforest=cforest_pred,
                   svm=svm_pred,
                   xgb=xgb_pred,
                   gp=gp_pred,
                   nb=nb_pred,
                   knn=knn_pred,
                   # keras=keras_pred,
                   glm=glm_pred,
                   log=log_pred,
                   da=da_pred,
                   rrlda=rrlda_pred,
                   gbm=gbm_pred) %>%
  sapply(function(x) as.numeric(as.character(x)))

corrplot.mixed(cor(combine),upper = 'ellipse')

rf_sensemble_model <- randomForest(Survived~.,combine)


# gbm
(gbm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='gbm'))

gbm_test_pred <- predict(gbm,test)
rm(gbm)

# rrlda
(rrlda<-train(Survived~.,data=train,
              trControl=trainControl(method="repeatedcv", number=5, repeats=5),
              method='rrlda'))

rrlda_test_pred <- predict(rrlda,test)
rm(rrlda)

# da
(da<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='dda'))

da_test_pred <- predict(da,test)
rm(da)

# log
(log<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='regLogistic'))

log_test_pred <- predict(log,test)
rm(log)

# glm
(glm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='glmnet'))

glm_test_pred <- predict(glm,test)
rm(glm)


# keras
# (keras<-train(Survived~.,data=train,
#             trControl=trainControl(method="repeatedcv", number=5, repeats=5),
#             method='mlpKerasDropout'))
# 
# keras_test_pred <- predict(keras,test)
# 
# confusionMatrix(keras_test_pred,test$Survived)
# rm(keras)

# knn
(knn<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='kknn'))

knn_test_pred <- predict(knn,test)
rm(knn)

# Naive Bay
(nb<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='naive_bayes'))

nb_test_pred <- predict(nb,test)
rm(nb)

# Gaussian Process
(gp<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='gaussprPoly'))

gp_test_pred <- predict(gp,test)
rm(gp)

# svm
(svm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinearWeights'))

svm_test_pred <- predict(svm,test)
rm(svm)

# adaboost
(adaboost<-train(Survived~.,data=train,
                 trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                 method='adaboost'))

ada_test_pred <- predict(adaboost,test)
rm(adaboost)

# CForest
(cforest<-train(Survived~.,data=train,
                #trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                method='cforest',
                controls = cforest_unbiased(ntree = 1000)))
cforest_test_pred <- predict(cforest,test)
rm(cforest)

# xgboost
trControl <- trainControl(method="repeatedcv", number=5, repeats=5);
xgbGrid <- expand.grid(nrounds=c(30),
                       max_depth=c(8),
                       eta=c(0.1),
                       colsample_bytree=c(0.5),
                       subsample=c(1),
                       gamma=c(0),
                       min_child_weight=c(3))

(model.xgb <- train(Survived~.,data=train,trControl=trControl,method='xgbTree',
                    tuneGrid = xgbGrid))

xgb_test_pred <- predict(model.xgb,test)
rm(trControl,xgbGrid,model.xgb)

# combine
combine<-bind_cols(ada=ada_test_pred,
                   cforest=cforest_test_pred,
                   svm=svm_test_pred,
                   xgb=xgb_test_pred,
                   gp=gp_test_pred,
                   nb=nb_test_pred,
                   knn=knn_test_pred,
                   # keras=keras_test_pred,
                   glm=glm_test_pred,
                   log=log_test_pred,
                   da=da_test_pred,
                   rrlda=rrlda_test_pred,
                   gbm=gbm_test_pred) %>%
  sapply(function(x) as.numeric(as.character(x)))

# hard vote
(pred<-combine %>% as.data.frame() %>% 
    mutate(Survived=if_else(ada+svm+xgb+gp+nb+knn+rrlda>3,1,0)) %>% select(Survived))

# rf vote
pred<-if_else(predict(rf_sensemble_model,combine)>0.5,1,0)

solution <- data.frame(PassengerId = test$PassengerId,Survived = pred)



# .csv
write.csv(solution, file = 'ensemble_model.csv', row.names = F)
