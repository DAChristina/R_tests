# 2. Data Fitting ##############################################################
library(mcstate)
library(coda)
library(odin.dust)
library(dust)
library(GGally)
library(socialmixr)

library(future)
future::plan(multisession, workers = 4)

source("global/all_function_allAge_stochastic.R")
sir_data <- readRDS("mcstate/data.rds")
rmarkdown::paged_table(sir_data) # annotate so that it is suitable for the particle filter to use

gen_sir <- odin.dust::odin_dust("mcstate/model.R")

# This is part of sir odin model:
pars <- list(log_A_ini = (-5.69897), # S_ini*10^(log10(-5.69897)) = 120 people; change A_ini into log10(A_ini)
             time_shift_1 = 0.2,
             time_shift_2 = 0.2,
             beta_0 = 0.06565,
             beta_1 = 0.07, # in toy data the real value of beta_1 = 0.07
             beta_2 = 0.2,
             max_wane = (-0.5),
             min_wane = (-4),
             scaled_wane = (0.5),
             log_delta = (-4.98),
             sigma_2 = 1,
             
             alpha = 1,
             gamma_annual = 1,
             nu_annual = 1,
             
             kappa_Ne = 1,
             kappa_12F = 1,
             kappa_55 = 1
)









