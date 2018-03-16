# full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>%
#   arrange(desc(ppl_ticket)) %>% right_join(full,by='Ticket') %>% filter(FamilySize<ppl_ticket) %>%
#   arrange(Ticket) %>% group_by(Ticket) %>%
#   summarise(survival_rate=mean(Survived,na.rm=T),num_ppl=n()) %>% View()

#full <- full %>% group_by(Ticket) %>% summarise(ppl_ticket=n()) %>% 
# arrange(desc(ppl_ticket)) %>% right_join(full,by='Ticket')

#full$TravelSize <- ifelse(full$ppl_ticket>full$FamilySize,full$ppl_ticket,full$FamilySize)