setwd(here::here())
load("CPD.rda")
library(readxl)
mamo_data = read.csv("mamo_data.csv")
library(purrr)


# check if package logistf is installed, if not install it

if (!requireNamespace("logistf", quietly = TRUE)) {
  install.packages("logistf")
}

library(logistf)
library(dplyr)


# using some package to draw the correlation plot 

library(corrplot)

Data$BRCA -> brca

names(brca)

# [1] "Cancer Code"                "Patient ID"                 "Age at Diagnosis (Years)"   "Pathologic Stage"          
# [5] "Gender"                     "Apoptosis"                  "Breast Reactive"            "Cell Cycle"                
# [9] "Core Reactive"              "DNA Damage Response"        "EMT"                        "Hormone Receptor"          
# [13] "Hormone Signaling (Breast)" "PI3K/AKT"                   "RAS/MAPK"                   "RTK"                       
# [17] "TSC/mTOR"                   "Survival Indicator"  

# Rename those names with spaces

names(brca) <- make.names(names(brca))

brca_covar = brca %>% select(-Hormone.Receptor,
                             -Hormone.Signaling..Breast.,
                             -Cancer.Code,
                             -Core.Reactive)

brca_covar %>% psych::describe()

# checking na
map(brca_covar, ~sum(is.na(.)) )%>% unlist()

brca_covar_na <- brca_covar %>% select('Age.at.Diagnosis..Years.',
                                       'Pathologic.Stage',
                                       'Survival.Indicator')

brca_covar_na %>% filter(is.na(Age.at.Diagnosis..Years.)) %>% knitr::kable()
brca_covar_na %>% filter(is.na(Pathologic.Stage)) %>% knitr::kable()
brca_covar_na %>% filter(is.na(Survival.Indicator)) %>% knitr::kable()

# Check varable types

skimr::skim(brca_covar)

# correlation plot

corrplot(cor(brca_covar), method = "number")




