# gbm
(gbm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='gbm'))

gbm_pred <- predict(gbm,test)
rm(gbm)

# rrlda
(rrlda<-train(Survived~.,data=train,
              trControl=trainControl(method="repeatedcv", number=5, repeats=5),
              method='rrlda'))

rrlda_pred <- predict(rrlda,test)
rm(rrlda)

# da
(da<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='dda'))

da_pred <- predict(da,test)
rm(da)

# log
(log<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='regLogistic'))

log_pred <- predict(log,test)
rm(log)

# glm
(glm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='glmnet'))

glm_pred <- predict(glm,test)
rm(glm)


# keras
# (keras<-train(Survived~.,data=train,
#             trControl=trainControl(method="repeatedcv", number=5, repeats=5),
#             method='mlpKerasDropout'))
# 
# keras_pred <- predict(keras,test)
# 
# confusionMatrix(keras_pred,test$Survived)
# rm(keras)

# knn
(knn<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='kknn'))

knn_pred <- predict(knn,test)
rm(knn)

# Naive Bay
(nb<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='naive_bayes'))

nb_pred <- predict(nb,test)
rm(nb)

# Gaussian Process
(gp<-train(Survived~.,data=train,
           trControl=trainControl(method="repeatedcv", number=5, repeats=5),
           method='gaussprPoly'))

gp_pred <- predict(gp,test)
rm(gp)

# svm
(svm<-train(Survived~.,data=train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinearWeights'))

svm_pred <- predict(svm,test)
rm(svm)

# adaboost
(adaboost<-train(Survived~.,data=train,
                 trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                 method='adaboost'))

ada_pred <- predict(adaboost,test)
rm(adaboost)

# CForest
(cforest<-train(Survived~.,data=train,
                #trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                method='cforest',
                controls = cforest_unbiased(ntree = 1000)))
cforest_pred <- predict(cforest,test)
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

xgb_pred <- predict(model.xgb,test)
rm(trControl,xgbGrid,model.xgb)

# combine
combine<-bind_cols(ada=ada_pred,
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

