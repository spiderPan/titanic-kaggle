
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
