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



library(VGAM)
set.seed(123)  
n <- 1000  
x1 <- runif(n, -1, 1)
x2 <- rnorm(n)

p <- exp(0.5 + 2 * x1 - 1.5 * x2) / (1 + exp(0.5 + 2 * x1 - 1.5 * x2))
y <- rbinom(n, 1, p)  

table(y)

gev_model <- vglm(y ~ x1 + x2, family = gevff(), data = data.frame(x1, x2, y))
summary(gev_model)


gev_model <- vglm(y ~ x1 + x2, family = gevff(link = "loglog"), data = data.frame(x1, x2, y))
summary(gev_model)













