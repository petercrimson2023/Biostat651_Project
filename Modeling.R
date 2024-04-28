#_______________________Reading data_______________________#

setwd(here::here())
load("CPD.rda")
library(dplyr)
library(ggplot2)

#_______________________Study Cohort Construction_______________________#

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


data_bind_2 = rbind(kirc,lusc)


#_______________________Modeling_______________________#

#Logistic Regresison

# Reference Model (Model 1)

glm(
  Survival.Indicator ~ Gender + Age.at.Diagnosis..Years. + state_1 + state_2,
  data = data_bind_2,
  family = binomial
) -> temp_model

summary(temp_model)

# Combined Effect Model (Model 2)

glm(
  Survival.Indicator ~ Apoptosis *  Cancer.Code + Gender + Age.at.Diagnosis..Years. + state_1 + state_2,
  data = data_bind_2,
  family = binomial
) %>% summary()


# Subgroup analysis (Model 3)

glm(
  Survival.Indicator ~ (Apoptosis+ Gender + Age.at.Diagnosis..Years. + state_1 + state_2) * Cancer.Code,
  data = data_bind_2,
  family = binomial
) %>% summary()









