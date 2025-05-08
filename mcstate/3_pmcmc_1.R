# 2. Data Fitting ##############################################################
library(mcstate)
library(coda)
library(odin.dust)
library(dust)
library(GGally)
library(tidyverse)

source("mcstate/all_function.R")
sir_data <- readRDS("mcstate/data.rds")
rmarkdown::paged_table(sir_data) # annotate so that it is suitable for the particle filter to use

## 2a. Model Load ##############################################################
# The model below is stochastic, closed system SADR model that I have created before
# I updated the code, filled the parameters with numbers;
# e.g.dt <- user(0) because if dt <- user() generates error during MCMC run
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
             log_delta = (-4.98)
)

priors <- prepare_priors(pars)
proposal_matrix <- diag(300, 8) # previously 200
proposal_matrix <- (proposal_matrix + t(proposal_matrix)) / 2
rownames(proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta")
colnames(proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta")

mcmc_pars <- prepare_parameters(initial_pars = pars,
                                priors = priors,
                                proposal = proposal_matrix,
                                transform = transform)


# Check the transform function:
# transform(mcmc_pars$initial())

# n_steps <- 100 #1e6

# I change pmcmc_run into a function that involve control inside:
# pmcmc_run <- mcstate::pmcmc(mcmc_pars, filter_deterministic, control = control)

# Directory for saving the outputs
# dir.create("outputs/genomics/trial_deterministic_1000", FALSE, TRUE)

# Trial combine pMCMC + tuning #################################################
pmcmc_run1 <- function(n_pars, n_sts){
  dir_name <- paste0("mcstate/", n_sts, "/")
  dir.create(dir_name, FALSE, TRUE)
  filter <- mcstate::particle_filter$new(data = sir_data,
                                         model = gen_sir, # Use odin.dust input
                                         n_particles = n_pars,
                                         compare = case_compare,
                                         seed = 1L)
  
  # Use deterministic model by add filter_deterministic
  # https://mrc-ide.github.io/mcstate/articles/deterministic.html
  # Index function is optional when only a small number of states are used in comparison function.
  filter_deterministic <- mcstate::particle_deterministic$new(data = sir_data,
                                                              model = gen_sir,
                                                              compare = case_compare,
                                                              index = index_fun
  )
  
  
  control <- mcstate::pmcmc_control(n_steps = n_sts,
                                    rerun_every = 50,
                                    rerun_random = TRUE,
                                    progress = TRUE)
  
  # The pmcmc
  pmcmc_result <- mcstate::pmcmc(mcmc_pars, filter_deterministic, control = control)
  pmcmc_result
  saveRDS(pmcmc_result, paste0(dir_name, "pmcmc_result.rds"))
  
  new_proposal_mtx <- cov(pmcmc_result$pars)
  write.csv(new_proposal_mtx, paste0(dir_name, "new_proposal_mtx.csv"), row.names = FALSE)
  
  lpost_max <- which.max(pmcmc_result$probabilities[, "log_posterior"])
  write.csv(as.list(pmcmc_result$pars[lpost_max, ]),
            paste0(dir_name, "initial.csv"), row.names = FALSE)
  
  # Further processing for thinning chains
  mcmc1 <- pmcmc_further_process(n_sts, pmcmc_result)
  write.csv(mcmc1, paste0(dir_name, "mcmc1.csv"), row.names = FALSE)
  
  # Calculating ESS & Acceptance Rate
  calc_ess <- ess_calculation(mcmc1)
  write.csv(calc_ess, paste0(dir_name, "calc_ess.csv"), row.names = FALSE)
  
  # Figures! (still failed, margin error)
  fig <- pmcmc_trace(mcmc1)
  
  Sys.sleep(10) # wait 10 secs before conducting tuning
}

# a slight modification for college's HPC
args <- commandArgs(trailingOnly = T)
n_pars <- as.numeric(args[which(args == "--n_particles") + 1])
n_sts <- as.numeric(args[which(args == "--n_steps") + 1])

pmcmc_run1(n_pars, n_sts)
