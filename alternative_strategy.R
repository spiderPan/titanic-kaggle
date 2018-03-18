# full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>%
#   arrange(desc(ppl_ticket)) %>% right_join(full,by='Ticket') %>% filter(FamilySize<ppl_ticket) %>%
#   arrange(Ticket) %>% group_by(Ticket) %>%
#   summarise(survival_rate=mean(Survived,na.rm=T),num_ppl=n()) %>% View()

#full <- full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>% 
# arrange(desc(ppl_ticket)) %>% right_join(full,by='Ticket')

#full$TravelSize <- ifelse(full$ppl_ticket>full$FamilySize,full$ppl_ticket,full$FamilySize)

#train(Survived~.,data=train,trControl=trainControl(method='cv',number=10),method='rf')

## Decision Tree
dt_model<-rpart(Survived~.,train)
fancyRpartPlot(dt_model)
rpart.plot(dt_model)
dt_pred <- predict(dt_model,test,type="class")

varImp(dt_model)

## svm
test$Survived <- NULL
svm_model <- svm(Survived~.,train)
svm_pred <- predict(svm_model,test)


## RandomForest
rf_model <- randomForest(Survived~.,train)
rf_pred <- predict(rf_model,test)

cbind(dt_pred,svm_pred,rf_pred)

importance(rf_model) %>% as.data.frame() %>% rownames_to_column("Var") %>% 
  ggplot(aes(reorder(Var,MeanDecreaseGini),MeanDecreaseGini,fill=MeanDecreaseGini))+geom_col()+
  coord_flip()+theme_few()+labs(x='',y='')+theme(legend.position = 'none')

varImpPlot(rf_model)


combine<-bind_cols(cf=cforest_pred,svm=svm_pred,xgb=xgb_pred)



