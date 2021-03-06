library(tidyverse)
library(caret)
library(corrplot)
library(randomForest)
library(gridExtra)
library(rebus)

training <- read_csv('data/train.csv')
testing <- read_csv('data/test.csv')

training <- train
testing <- test
  
str(training)

str(testing)


full <- bind_rows(training, testing)
summary(full)
head(full)
tail(full)


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

full[1:nrow(training),] %>% ggplot(aes(Survived, fill=Survived))+geom_bar()+
  facet_grid(.~Title)+theme(legend.position = 'none')

full$Title %>% table()
full[1:nrow(training),] %>% filter(Title=='Col') %>% nrow()

full1 <- full

full$Title <- as.character(full$Title)

full1$Title[full1$Title %in% c("Capt",'Col','Don','Dona','Jonkheer','Lady','Major','Mlle','Mme',
                               'Ms','Rev','Dr','Sir','the Countess')]<-"Rare"

full$Ticket


full$Ticket2<-full1$Ticket

full[,c("Embarked","Ticket","Ticket2")] %>% filter(Ticket2>35, Ticket2<50) %>% unique() %>% View()


(m<-full[1:891,] %>% ggplot(aes(Ticket2,fill=factor(Survived)))+
    geom_histogram(position = 'fill')+theme(legend.position='none'))
(n<-full1[1:891,] %>% filter(!Ticket %in% c(63,837)) %>% 
    select(Ticket,Embarked) %>% unique() %>% 
    ggplot(aes(Ticket,Embarked))+geom_col())
grid.arrange(n,m)



str(full1)

summary(full1)

full[is.na(full1$Embarked),] %>% str()

full[is.na(full1$Fare),] %>% str()

full1 %>% filter(Cabin==29) %>% str()


full1 %>% group_by(Embarked) %>% summarise(count=n())

full1 %>% select(Embarked,Pclass) %>% table()


full %>% select(Ticket,Embarked) %>% table()
full %>% filter(!Ticket %in% c(63,837)) %>% select(Ticket,Embarked) %>% unique() %>% 
  ggplot(aes(Ticket,Embarked))+geom_col()

full %>% select(Ticket,Embarked) %>% unique() %>% filter(Ticket<60) %>% 
  ggplot(aes(Ticket,Embarked))+geom_col()+geom_vline(aes(xintercept=43),col='firebrick',lty=2,size=2)

full1 %>% select(Ticket,Embarked) %>% unique() %>% 
  ggplot(aes(Ticket,Embarked))+geom_point(size = 3,position = 'jitter',shape = 1,alpha = .4)



full1 %>% select(Ticket,Embarked) %>% unique() %>% group_by(Ticket) %>% count() %>% arrange(desc(n))

full1[full1$Ticket %in% c(63,837),] %>% View()


full1[is.na(full$Fare),]

full1 %>% group_by(Pclass) %>% select(Fare) %>% summarise(med=median(Fare,na.rm=T))

a <- full1 %>% filter(Pclass==3) %>% ggplot(aes(Ticket,Fare))+geom_point(size=4,col='firebrick',shape=1,alpha=.4)+
  geom_hline(aes(yintercept=median(Fare,na.rm=T)),col='darkblue',lty=2)+
  geom_vline(xintercept = 632,col='steelblue',lty=2)

b <- full1[full1$Embarked==3 & full1$Pclass==3,] %>% ggplot(aes(Ticket))+geom_histogram()+
  geom_vline(xintercept = 632,col='steelblue',lty=2)+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        panel.grid = element_blank(),plot.margin = unit(c(0,0.03,-0.85,0.64),"cm"))+
  labs(x='',y='')

c <- full1[full1$Embarked==3 & full1$Pclass==3,] %>% ggplot(aes(Fare))+geom_density(fill='lightblue',alpha=.5)+
  geom_vline(aes(xintercept = median(Fare,na.rm=T)),col='darkblue',lty=2)+
  coord_flip()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
                     panel.background = element_blank(),panel.border = element_blank(),
                     panel.grid = element_blank(),plot.margin = unit(c(0.34,0,0.52,-.9),"cm"))+
  labs(x='',y='')

Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
  labs(x="",y="")+
  annotate('text', x = 0, y = 0, label = "Fare vs. Ticket\nby MultiGraph",size=4)+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm"))

grid.arrange(a,b,c,Title,layout_matrix=
               t(matrix(c(2,2,2,4,
                          1,1,1,3,
                          1,1,1,3,
                          1,1,1,3),nrow=4)))






# full1[full1$Embarked==3 & full1$Pclass==3 & full1$Fare>40,] %>% View()
full <- bind_rows(training, testing)
full %>% filter(!is.na(Cabin)) %>% select(Cabin,Ticket,Pclass,Fare) %>% View()

str(full)
sapply(full,function(x) sum(is.na(x)))

str(full1)

non_na_cabin<-full %>% filter(!is.na(Cabin)) %>% select('Cabin','Ticket','FamilyId')

combined <- full %>% left_join(non_na_cabin,by='FamilyId')

full$FamilyId<-full1$IsFamily

New_Cabin<-full %>% filter(!is.na(Cabin)) %>% 
  mutate(CabinCN=if_else(is.na(str_extract(Cabin,WRD %R% one_or_more(DGT))),Cabin,str_extract(Cabin,WRD %R% one_or_more(DGT))),
         CabinC=str_extract(Cabin,WRD),
         CabinN=str_extract(Cabin,optional(DGT) %R% optional(DGT) %R% optional(DGT) %R% END))

str(New_Cabin)
summary(New_Cabin)


New_Cabin %>% 




inTrain = createDataPartition(training$Survived, p = 3 / 4)[[1]]
training_set = training[inTrain,]
testing_set = training[-inTrain,]

numeric_cols = na.omit(full[, list])
corrplot(cor(numeric_cols), method = "circle")




randomForest(Survived~.,data=training_set)
