#output Data$BRCA as csv

# write.csv(Data$BRCA %>% as.data.frame(), "brca_covar_na.csv")
# 
# 
# names(data_bind) <- make.names(names(data_bind))
# 
# data_bind$Pathologic.Stage %>% table()
# 
# 
# 
# data_bind <- data_bind %>% mutate(
#   stage_1 = ifelse(Pathologic.Stage %in% c("i", "ia", "ib"), 1, 0),
#   stage_2 = ifelse(Pathologic.Stage %in% c("ii", "iia", "iib"), 1, 0),
#   stage_3 = ifelse(Pathologic.Stage %in% c("iii", "iiia", "iiib", "iv"), 1, 0),
# ) %>% filter(Pathologic.Stage != "x")
# 
# data_bind <-
#   data_bind %>% filter(!(Cancer.Code == "LUSC" & Gender ==))
# 
# 
# logistf(
#   data = data_bind,
#   formula = Survival.Indicator ~ -1 + EMT * Cancer.Code + Gender +
#     Age.at.Diagnosis..Years. +
#     stage_2 + stage_3 + stage_1
# ) %>% summary()




kirc <- Data$KIRC

names(kirc) <- make.names(names(kirc))

kirc <- kirc %>% select(
  -Patient.ID,
  -Hormone.Receptor
) %>% na.omit()

kirc <- kirc %>% mutate(
  state_1 = ifelse(Pathologic.Stage %in% c("i","ia","ib"), 1, 0),
  state_2 = ifelse(Pathologic.Stage %in% c("ii","iia","iib"),1,0)
) %>% select(-Pathologic.Stage)

# glm(Survival.Indicator ~ .,data=kirc,family = binomial) %>% summary()


lusc <- Data$LUSC

names(lusc) <- make.names(names(lusc))

lusc <- lusc %>% select(
  -Patient.ID,
  -Hormone.Receptor
) %>% na.omit()

lusc <- lusc %>% mutate(
  state_1 = ifelse(Pathologic.Stage %in% c("i","ia","ib"), 1, 0),
  state_2 = ifelse(Pathologic.Stage %in% c("ii","iia","iib"),1,0)
) %>% select(-Pathologic.Stage)

# glm(Survival.Indicator ~ .,data=lusc,family = binomial) %>% summary()




data_bind_2 = rbind(kirc,lusc)

# data_bind_2$Cancer.Code %>% table()



glm(
  Survival.Indicator ~ Apoptosis * Cancer.Code + Gender + Age.at.Diagnosis..Years. + state_1 + state_2,
  data = data_bind_2,
  family = binomial
) %>% summary()





glm(
  Survival.Indicator ~ .+ Apoptosis * Cancer.Code,
  data = data_bind_2,
  family = binomial
) %>% summary()



library(ggplot2)




data_bind_2 %>% ggplot(aes(x = Apoptosis, fill = Cancer.Code)) +
  geom_histogram(position = "dodge", bins = 30) +
  theme_minimal() +
  theme(legend.position = "top")


data_bind_2 %>% ggplot(aes(x = (Apoptosis-mean(Apoptosis)), fill = factor(Survival.Indicator))) + geom_histogram(position = "stack", bins = 30) + facet_wrap( ~ Cancer.Code, ncol = 1) + labs(x = "Apoptosis", y = "Count", fill = "Survival Indicator") + scale_fill_manual(
  values = c("0" = "blue", "1" = "red"),
  labels = c("Short Survivor", "Long Survivor")
) + theme_minimal() + theme(legend.position = "top")









