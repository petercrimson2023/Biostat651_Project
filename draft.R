setwd(here::here())

load("CPD.rda")

library(readxl)

mamo_data = read.csv("mamo_data.csv")


library(purrr)

# use map to check na for mamo_data`s each column

map(mamo_data, ~sum(is.na(.))) %>% unlist()

# use map to check na for CPD$`s each column

map(Data$BRCA, ~sum(is.na(.))) %>% unlist()
map(Data$LUSC, ~sum(is.na(.))) %>% unlist()
map(Data$KIRC, ~sum(is.na(.))) %>% unlist()
map(Data$LGG, ~sum(is.na(.))) %>% unlist()

# check main outcome 2 of mamo_data

table(mamo_data$stagefwup)
