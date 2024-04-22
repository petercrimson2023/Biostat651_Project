setwd(here::here())
load("CPD.rda")
library(readxl)
mamo_data = read.csv("mamo_data.csv")
library(purrr)

if (!requireNamespace("logistf", quietly = TRUE)) {
  install.packages("logistf")
}
library(logistf)
library(dplyr)
library(corrplot)
Data$LUSC -> lusc
map(lusc, ~sum(is.na(.)) )%>% unlist()



names(lusc) <- make.names(names(lusc))

skimr::skim(lusc)

lusc %>% select(Age.at.Diagnosis..Years.,
                Survival.Indicator,
                Gender,
                Pathologic.Stage) -> temo_lusc


# check the conditions of na columns verses survival indicator and gender

temo_lusc %>% filter(is.na(Age.at.Diagnosis..Years.)) %>% knitr::kable()

temo_lusc %>% filter(is.na(Survival.Indicator)) %>% knitr::kable()

temo_lusc %>% filter(is.na(Pathologic.Stage)) %>% knitr::kable()



# if (requireNamespace("glmnet", quietly = TRUE))
# {
#   install.packages("glmnet")
# }

library(glmnet)

lusc_complete <- lusc %>% filter(!is.na(Age.at.Diagnosis..Years.),
                                  !is.na(Survival.Indicator),
                                  !is.na(Pathologic.Stage))

X = lusc_complete %>% select(-Survival.Indicator,-Patient.ID,-Cancer.Code)

y = lusc_complete$Survival.Indicator

X$Pathologic.Stage %>% table()

# Creating stage_variables

X = X %>% mutate(
  stage_1 = ifelse(Pathologic.Stage %in% c("i","ia","ib"), 1, 0),
  stage_2 = ifelse(Pathologic.Stage %in% c("ii","iia","iib"), 1, 0),
  Gender = ifelse(X$Gender == "male",1,0)
)

X = X %>% select(-Pathologic.Stage)

# Creating interaction variables 

names(X)[3:14] -> covar_lists

for(i in 1:(length(covar_lists)-1))
{
  for(j in (i+1):length(covar_lists))
  {
    
      X[[paste0(covar_lists[i],"_",covar_lists[j])]] <- X[[covar_lists[i]]] * X[[covar_lists[j]]]
    
  }
}

names(X)

#glmnet(x = as.matrix(X),y=y,alpha = 1, family = "binomial")-> lusc_model

#plot(lusc_model, xvar = "lambda", label = TRUE)

# _____________________________________elastic net_________________________________

cv.glmnet(x=as.matrix(X), y, alpha = 0.5)
 
glmnet(x = as.matrix(X),y=y,alpha = 0.5, family = "binomial",lambda = c(1:5000)/(1E6))-> lusc_model
 
plot(lusc_model, xvar = "lambda", label = TRUE)


# non zero coefficients whrn lambda = 0.04484

coef(lusc_model, s = 0.03091)


# bootstraping for standard error estimation 

sampler <- function(data,num)
{
  n = nrow(data)
  samples = sample(1:n, n*num, replace = TRUE) %>% matrix(.,nrow = num)
  sampled_data = apply(samples,1,function(x) data[x,])
  return(sampled_data)
}


bootstrap_sample = sampler(cbind(X,y),3000)


sapply(bootstrap_sample,function(data){
  y <- pull(data, y)
  x <- select(data, -y)
  model_temp = glmnet(
    x = as.matrix(x),
    y = y,
    alpha = 0.5,
    family = "binomial",
    lambda = 0.04484
  )
  coef_temp = coef(model_temp) %>% as(., "matrix")
  return(coef_temp)
}) -> boot_coef

boot_coef %>% t() %>% apply(2, sd) -> boot_se

boot_se = (boot_se + 1e-10) 

coef(lusc_model, s = 0.04484) %>% as(., "matrix") -> coef_mat

interval = cbind(coef_mat - 1.96 * boot_se, coef_mat + 1.96 * boot_se)

names(interval) <- c("lower", "upper")

interval %>% as.data.frame() -> interval_df



# plotting forest plot 

plot_data <- cbind(coef_mat, interval_df)
colnames(plot_data) <- c("Estimate", "CI_low", "CI_high")


plot_data$variable <- rownames(plot_data)
plot_data$variable <- factor(plot_data$variable, levels = plot_data$variable)


ggplot(plot_data, aes(x = Estimate, y = variable)) +
  geom_point() +                                           
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high)) +    
  labs(x = "Coefficient Estimate", y = "Variables") +     
  theme_bw() +                                            
  theme(axis.text.y = element_text(size = 8)) 

interval_df %>% ptiny()


#____________________________________ lasso _________________________________




cv.glmnet(x=as.matrix(X), y, alpha = 1)

glmnet(x = as.matrix(X),y=y,alpha = 1, family = "binomial",lambda = c(1:5000)/(1E6))-> lusc_model

plot(lusc_model, xvar = "lambda", label = TRUE)


# non zero coefficients whrn lambda = 0.03918

coef(lusc_model, s = 0.03918)

lusc_finale = glmnet(
  x = as.matrix(X),
  y = y,
  alpha = 1,
  family = "binomial",
  lambda = 0.03918
)

y_pred_lasso = predict(lusc_finale,
                       s = 0.03918,
                       newx = as.matrix(X),
                       type = "response")


table(y,y_pred_lasso > 0.1)

# bootstraping for standard error estimation 

sampler <- function(data,num)
{
  n = nrow(data)
  samples = sample(1:n, n*num, replace = TRUE) %>% matrix(.,nrow = num)
  sampled_data = apply(samples,1,function(x) data[x,])
  return(sampled_data)
}


bootstrap_sample = sampler(cbind(X,y),1000)


sapply(bootstrap_sample,function(data){
  y <- pull(data, y)
  x <- select(data, -y)
  model_temp = glmnet(
    x = as.matrix(x),
    y = y,
    alpha = 1,
    family = "binomial",
    lambda =0.03918
  )
  coef_temp = coef(model_temp) %>% as(., "matrix")
  return(coef_temp)
}) -> boot_coef

boot_coef %>% t() %>% apply(2, sd) -> boot_se

boot_se = (boot_se + 1e-10) 

coef(lusc_model, s = 0.04484) %>% as(., "matrix") -> coef_mat

interval = cbind(coef_mat - 1.96 * boot_se, coef_mat + 1.96 * boot_se)

names(interval) <- c("lower", "upper")

interval %>% as.data.frame() -> interval_df

interval_df

# plotting forest plot 

plot_data <- cbind(coef_mat, interval_df)
colnames(plot_data) <- c("Estimate", "CI_low", "CI_high")


plot_data$variable <- rownames(plot_data)
plot_data$variable <- factor(plot_data$variable, levels = plot_data$variable)


ggplot(plot_data, aes(x = Estimate, y = variable)) +
  geom_point() +                                           
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high)) +    
  labs(x = "Coefficient Estimate", y = "Variables") +     
  theme_bw() +                                            
  theme(axis.text.y = element_text(size = 8)) 



#____________________________________ usual logistic regression ________________________________

lusc_complete <- lusc %>% filter(!is.na(Age.at.Diagnosis..Years.),
                                 !is.na(Survival.Indicator),
                                 !is.na(Pathologic.Stage))

X_single = lusc_complete %>% select(-Survival.Indicator,-Patient.ID,-Cancer.Code)

y = lusc_complete$Survival.Indicator

# Creating stage_variables

X_single = X_single %>% mutate(
  stage_1 = ifelse(Pathologic.Stage %in% c("i","ia","ib"), 1, 0),
  stage_2 = ifelse(Pathologic.Stage %in% c("ii","iia","iib"), 1, 0),
  Gender = ifelse(X_single$Gender == "male",1,0)
)

X_single = X_single %>% select(-Pathologic.Stage)

# using IQR normalization only on 2:ncol(X_single)-2

#X_single[,3:(ncol(X_single)-2)] = (X_single[,3:(ncol(X_single)-2)] - apply(X_single[,3:(ncol(X_single)-2)],2,median))/apply(X_single[,3:(ncol(X_single)-2)],2,IQR) 

#X_single = (X_single - apply(X_single,2,median))/apply(X_single,2,IQR)

#X_single = (X_single - apply(X_single,2,mean))/apply(X_single,2,sd)

X_single[,3:(ncol(X_single)-2)] = (X_single[,3:(ncol(X_single)-2)] - apply(X_single[,3:(ncol(X_single)-2)],2,mean))/apply(X_single[,3:(ncol(X_single)-2)],2,sd)

complete_data = cbind(X_single,y)

glm_model <- glm(y ~ ., data = complete_data, family = "binomial")

summary(glm_model)




#____________________________________ using BART to do variable selection _________________________________

library(dbarts)

x_train = X
y_train = y
x_test  = NULL

bart_model = bart(x_train, y_train, x_test)

var_counts <- bart_model$varcount
var_counts[900:1000,] -> var_counts_last

var_counts_df <- data.frame(var_counts_last) %>% colMeans() %>% as.data.frame()
var_counts_df$variable <- rownames(var_counts_df)
colnames(var_counts_df) <- c("count", "variable")
var_counts_df <- var_counts_df[order(var_counts_df[,1], decreasing = TRUE), ]
print(var_counts_df)


ggplot(var_counts_df, aes(x = reorder(variable, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Count") +
  theme_minimal()

# see the prediction of bart

bart_model$yhat.train -> bart_pred











