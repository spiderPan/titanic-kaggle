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


# Fare
full<-full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>%
  right_join(full,by='Ticket') %>% mutate(Adj_Fare=Fare/ppl_ticket)

full %>% View()

str(full)

full[is.na(full1$Fare),] %>% View()

full %>% filter(Pclass==3) %>% ggplot(aes(Adj_Fare))+
  geom_density(size=.5,alpha=.5,fill='grey')+
  geom_vline(aes(xintercept=median(Adj_Fare,na.rm=T)),lty=2,lwd=1.5,col='firebrick')+
  theme_few()
full %>% filter(Pclass==3) %>% select(Adj_Fare) %>% summary()

# Age
full %>% filter(is.na(Age)) %>% group_by(Title) %>% summarise(num_ppl=n())

full<-full %>% filter(!is.na(Age)) %>% group_by(Title) %>% 
  summarise(avg_age=median(Age)) %>% right_join(full,by='Title') %>% 
  mutate(Adj_Age=if_else(is.na(Age),avg_age,Age))

rm(full1)



