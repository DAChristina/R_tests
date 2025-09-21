# Test script using dataset downloaded on:
# https://www.kaggle.com/datasets/ankushpanday2/oral-cancer-prediction-dataset

library(tidyverse)
library(epitools)

dat <- read.csv("Epi_tests/raw_data/oral_cancer_prediction_dataset.csv")
colnames(dat)
view(dat)
summary(dat)


# odds ratio (for each factor) --> logistic regression analysis
# multivariate logistic regression (cool!)

# causal inference analysis
# relative difference in incidence


dat$Oral_Cancer_Diagnosis <- as.factor(dat$Oral.Cancer..Diagnosis.)

# Run logistic regression
model <- glm(Oral_Cancer_Diagnosis ~ Tobacco.Use + Alcohol.Consumption + HPV.Infection + Betel.Quid.Use, 
             data = dat, 
             family = binomial)

# Extract odds ratios and confidence intervals
exp(cbind(OR = coef(model), confint(model)))

################################################################################
# using epitools
table1 <- table(dat$Tobacco.Use, dat$Oral_Cancer_Diagnosis)
epitools::oddsratio(table1)

################################################################################
# multivariate analysis
model_adj <- glm(Oral_Cancer_Diagnosis ~ Tobacco.Use + Alcohol.Consumption + HPV.Infection + 
                   Betel.Quid.Use + Age + Gender + Poor.Oral.Hygiene + Diet..Fruits...Vegetables.Intake., 
                 data = dat, 
                 family = binomial)

summary(model_adj)
exp(cbind(OR = coef(model_adj), confint(model_adj)))

# checking multicollinearity for two or more predictors that are strongly correlated???
library(car)
vif(model_adj)

# outliers
cooks.distance(model_adj)

# model fit
library(ResourceSelection)
hoslem.test(model_adj$y, fitted(model_adj))

# pseudo R^2 (McFadden's R^2) --> logistic regression dosen't have R^2 like linear regression
library(pscl)
pR2(model_adj)

# multivariate analyses for genome
model_genome <- glm(Oral_Cancer_Diagnosis ~ Cancer_genome_types + Age + Gender + Tobacco.Use + 
                      Alcohol.Consumption + HPV.Infection, 
                    data = OralCancerData, 
                    family = binomial)

exp(cbind(OR = coef(model_genome), confint(model_genome)))

library(nnet)
model_multi <- multinom(Cancer_genome_types ~ Age + Gender + Tobacco.Use + Alcohol.Consumption + HPV.Infection, 
                        data = OralCancerData)
summary(model_multi)


# Linear regression
model_tumor <- lm(Tumor.Size..cm. ~ Cancer_genome_types + Age + Gender + Tobacco.Use, data = OralCancerData)
summary(model_tumor)




################################################################################
# Kaplan-Meier curves coz we have 5-year survival rate
library(survival)
surv_object <- Surv(time = dat$Survival.Rate..5.Year...., event = dat$Oral_Cancer_Diagnosis)

# Cox regression
cox_model <- coxph(surv_object ~ Tobacco.Use + Alcohol.Consumption + HPV.Infection, data = dat)

summary(cox_model)


################################################################################
# Trial ML using random forest XD
library(randomForest)
dat$Oral.Cancer..Diagnosis. <- as.factor(dat$Oral.Cancer..Diagnosis.)

cat("CAUTION! VERY LONG RUNNING TIME!")
rf_model <- randomForest(Oral.Cancer..Diagnosis. ~ ., data = dat, importance = TRUE)
varImpPlot(rf_model)


# Lasso regression
library(glmnet)
x <- model.matrix(Oral.Cancer..Diagnosis. ~ ., dat)[, -1]  # Convert predictors to matrix
y <- dat$Oral.Cancer..Diagnosis.
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(lasso_model)
coef(lasso_model, s = "lambda.min")


















# 1a. Meningitis-death odd ratio for all data ##################################
# (Death+ & Meningitis+)
all_Dm <- dat_G %>% 
  filter(MeningitisFlag == "Y" & !is.na(`30daydeath`)) %>% # not NA equals to "D"
  nrow()

# (Death+ & Meningitis-)
all_Dmmin <- dat_G %>% 
  filter(MeningitisFlag == "N" & !is.na(`30daydeath`)) %>% # not NA equals to "D"
  nrow()

# (Death- & Meningitis+)
all_Dminm <- dat_G %>% 
  filter(MeningitisFlag == "Y" & is.na(`30daydeath`)) %>% 
  nrow()

# (Death- & Meningitis-)
all_Dminmmin <- dat_G %>% 
  filter(MeningitisFlag == "N" & is.na(`30daydeath`)) %>% 
  nrow()

# Matrix (D+M+, D-M+, D+M-, D-M-)
Outcome <- c("Death", "Alive")
Meningitis <- c("Meningitis", "Not Meningitis")


Input_mD <- matrix(c(all_Dm, all_Dminm,
                     all_Dmmin, all_Dminmmin),
                   nrow = 2, ncol = 2, byrow = T)
dimnames(Input_mD) <- list('Meningitis'=Meningitis, 'Outcome'=Outcome)

All_OD <- epitools::oddsratio(Input_mD)
All_OD

# 1b. Meningitis-death odd ratio grouped by ageGroup ###########################
# Sort ageGroup before analysis
sortedAgeG <- c("<2", "2-4", "5-14", "15-30", "31-44", "45-64", "65+", "Unknown")
uniqAgeG <- unique(dat_G$ageGroup)
uniqAgeG
uniqAgeG <- uniqAgeG[match(sortedAgeG, uniqAgeG)]
uniqAgeG

aged <- vector("list", length(uniqAgeG)) # place to the filtered df
Input_mD <- vector("list", length(uniqAgeG)) # place to store the matrices coz'errors happen
All_OD <- vector("list", length(uniqAgeG)) # place to store the OddRatio result

ODperAgeGroup_report <- data.frame(matrix(NA, nrow=length(uniqAgeG), ncol=7)) # 7 elements of data to be compiled


for (demo in seq_along(uniqAgeG)){
  aged[[demo]] <- dat_G %>% 
    filter(ageGroup == uniqAgeG[demo])
  
  # (Death+ & Meningitis+)
  all_Dm <- aged[[demo]] %>% 
    filter(MeningitisFlag == "Y" & !is.na(`30daydeath`)) %>% 
    nrow()
  
  # (Death+ & Meningitis-)
  all_Dmmin <- aged[[demo]] %>% 
    filter(MeningitisFlag == "N" & !is.na(`30daydeath`)) %>% 
    nrow()
  
  # (Death- & Meningitis+)
  all_Dminm <- aged[[demo]] %>% 
    filter(MeningitisFlag == "Y" & is.na(`30daydeath`)) %>% 
    nrow()
  
  # (Death- & Meningitis-)
  all_Dminmmin <- aged[[demo]] %>% 
    filter(MeningitisFlag == "N" & is.na(`30daydeath`)) %>% 
    nrow()
  
  # Matrix (D+M+, D-M+, D+M-, D-M-)
  Outcome <- c("Death", "Alive")
  Meningitis <- c("Meningitis", "Not Meningitis")
  
  Input_mD[[demo]] <- matrix(c(all_Dm, all_Dminm,
                               all_Dmmin, all_Dminmmin),
                             nrow = 2, ncol = 2, byrow = T)
  dimnames(Input_mD[[demo]]) <- list('Meningitis'=Meningitis, 'Outcome'=Outcome)
  
  # Try catch part coz some ageGroups can't be analysed by epitools:
  tryCatch({
    All_OD[[demo]] <- epitools::oddsratio(Input_mD[[demo]])
  },
  error = function(e) {
    # Print an error message instead
    cat("Error message:", conditionMessage(e), "\n")
    
    All_OD[[demo]] <- NA # Assign NA to indicate the error
  }
  )
  
}
# Trial print
# print(aged[[1]])
# print(Input_mD[[1]])
# print(All_OD[[1]])

# PRINT THE RESULTS!
for (demo in seq_along(uniqAgeG)){
  real_ages <- uniqAgeG[demo]
  cat("#########################################################################", "\n" )
  print(real_ages)
  print(Input_mD[[demo]]) # print the matrix coz error happens
  cat("\n")
  print(All_OD[[demo]])
  cat("\n", "\n")
  
}

# Update df report
for (i in ODperAgeGroup_report){
  ODperAgeGroup_report[i, 1] <- uniqAgeG[demo]
  ODperAgeGroup_report[i, 2] <- All_OD[[demo]]$measure[2]
  ODperAgeGroup_report[i, 3] <- All_OD[[demo]]$measure[4]
  ODperAgeGroup_report[i, 4] <- All_OD[[demo]]$measure[6]
  ODperAgeGroup_report[i, 5] <- All_OD[[demo]]$p.value[2]
  ODperAgeGroup_report[i, 6] <- All_OD[[demo]]$p.value[4]
  ODperAgeGroup_report[i, 7] <- All_OD[[demo]]$p.value[6]
  
}
# Test the df
glimpse(ODperAgeGroup_report)

# Error occur coz some ageGroups cannot be calculated, resulting in null data results
# Seems like I have to mannually fill the data -_-)
ODperAgeGroup_reportdf <- data.frame(ageGroup = character(length(uniqAgeG)), # [1] "<2"      "2-4"     "5-14"    "15-30"   "31-44"   "45-64"   "65+"     "Unknown"
                                     OddsRatio = integer(length(uniqAgeG)),
                                     OD_lo_CI = integer(length(uniqAgeG)),
                                     OD_hi_CI = integer(length(uniqAgeG)),
                                     p_midp.exact = integer(length(uniqAgeG)),
                                     p_fisher.exact = integer(length(uniqAgeG)),
                                     p_chi.sq = integer(length(uniqAgeG))
)
# Mannually update the data:
ODperAgeGroup_reportdf$ageGroup <- uniqAgeG
ODperAgeGroup_reportdf$OddsRatio <- c(All_OD[[1]]$measure[2], NA, NA, NA,
                                      All_OD[[5]]$measure[2],
                                      All_OD[[6]]$measure[2],
                                      All_OD[[7]]$measure[2], NA)

ODperAgeGroup_reportdf$OD_lo_CI <- c(All_OD[[1]]$measure[4], NA, NA, NA,
                                     All_OD[[5]]$measure[4],
                                     All_OD[[6]]$measure[4],
                                     All_OD[[7]]$measure[4], NA)

ODperAgeGroup_reportdf$OD_hi_CI <- c(All_OD[[1]]$measure[6], NA, NA, NA,
                                     All_OD[[5]]$measure[6],
                                     All_OD[[6]]$measure[6],
                                     All_OD[[7]]$measure[6], NA)

ODperAgeGroup_reportdf$p_midp.exact <- c(All_OD[[1]]$p.value[2], NA, NA, NA,
                                         All_OD[[5]]$p.value[2],
                                         All_OD[[6]]$p.value[2],
                                         All_OD[[7]]$p.value[2], NA)

ODperAgeGroup_reportdf$p_fisher.exact <- c(All_OD[[1]]$p.value[4], NA, NA, NA,
                                           All_OD[[5]]$p.value[4],
                                           All_OD[[6]]$p.value[4],
                                           All_OD[[7]]$p.value[4], NA)

ODperAgeGroup_reportdf$p_chi.sq <- c(All_OD[[1]]$p.value[6], NA, NA, NA,
                                     All_OD[[5]]$p.value[6],
                                     All_OD[[6]]$p.value[6],
                                     All_OD[[7]]$p.value[6], NA)

glimpse(ODperAgeGroup_reportdf)


# Trial apply this as a function (maybe later)
uniqAgeG <- unique(dat_G$ageGroup)

OddRatio_Fun <- function(demo){
  aged[[demo]] <- dat_G %>% 
    filter(ageGroup == demo)
  
  # (Death+ & Meningitis+)
  all_Dm[[demo]] <- dat_G %>% 
    filter(MeningitisFlag == "Y" & !is.na(`30daydeath`)) %>% # not NA equals to "D"
    nrow()
  
  # (Death+ & Meningitis-)
  all_Dmmin[[demo]] <- dat_G %>% 
    filter(MeningitisFlag == "N" & !is.na(`30daydeath`)) %>% # not NA equals to "D"
    nrow()
  
  # (Death- & Meningitis+)
  all_Dminm[[demo]] <- dat_G %>% 
    filter(MeningitisFlag == "Y" & is.na(`30daydeath`)) %>% 
    nrow()
  
  # (Death- & Meningitis-)
  all_Dminmmin[[demo]] <- dat_G %>% 
    filter(MeningitisFlag == "N" & is.na(`30daydeath`)) %>% 
    nrow()
  
  # Matrix (D+M+, D-M+, D+M-, D-M-)
  Outcome[[demo]] <- c("Death", "Alive")
  Meningitis[[demo]] <- c("Meningitis", "Not Meningitis")
  
  
  Input_mD[[demo]] <- matrix(c(all_Dm, all_Dminm,
                               all_Dmmin, all_Dminmmin),
                             nrow = 2, ncol = 2, byrow = T)
  dimnames(Input_mD[[demo]]) <- list('Meningitis'=Meningitis, 'Outcome'=Outcome)
  
  All_OD[[demo]] <- epitools::oddsratio(Input_mD[[demo]])
  print(All_OD[[demo]])
  
}

lapply(uniqAgeG, OddRatio_Fun)


# Quick notes:
# Better to create a separate df and group them first then count the indicence later.
# Something weird happened with sum() if we grouped and re-grouped the variables.









