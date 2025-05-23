# 2. Data Fitting ##############################################################
library(mcstate)
library(coda)
library(odin.dust)
library(dust)
library(GGally)

library(future)
future::plan(multisession, workers = 4)

source("global/all_function_allAge_stochastic.R")
sir_data <- readRDS("inputs/pmcmc_data_week_allAge.rds")
rmarkdown::paged_table(sir_data) # annotate so that it is suitable for the particle filter to use

## 2a. Model Load ##############################################################
# The model below is stochastic, closed system SADR model that I have created before
# I updated the code, filled the parameters with numbers;
# e.g.dt <- user(0) because if dt <- user() generates error during MCMC run
gen_sir <- odin.dust::odin_dust("model/sir_stochastic_allAge.R")

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

# https://mrc-ide.github.io/odin-dust-tutorial/mcstate.html#/the-model-over-time
# n_particles <- 50 # Trial n_particles = 50
# filter <- mcstate::particle_filter$new(data = sir_data,
#                                        model = gen_sir, # Use odin.dust input
#                                        n_particles = n_particles,
#                                        compare = case_compare,
#                                        seed = 1L)
# 
# filter$run(pars)

# Variance and particles estimation (as suggested by Rich)
# parallel::detectCores() # 4 cores
# x <- replicate(30, filter$run(pars))
# var(x)
# [1] 3520.937
# Trial 320000 particles to get var(x) = 1 on 4 chains/4 nodes/4 cores per-node
# (320000/4/4/4)/3520.937
# Trial 320000 particles to get var(x) = 1 on 4 chains/1 nodes/20 cores per-node
# 3520.937*(4*1*20) # ~ 281675
# (281675/4/1/20)/3520.937

# Update n_particles based on calculation in 4 cores with var(x) ~ 3520.937: 281675

priors <- prepare_priors(pars)
proposal_matrix <- diag(200, 14)
proposal_matrix <- (proposal_matrix + t(proposal_matrix)) / 2
rownames(proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta", "alpha", "gamma_annual", "nu_annual", "kappa_Ne", "kappa_12F", "kappa_55")
colnames(proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta", "alpha", "gamma_annual", "nu_annual", "kappa_Ne", "kappa_12F", "kappa_55")

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
pmcmc_run2 <- function(n_pars, n_sts){
  dir_name <- paste0("outputs/genomics/trial_deterministic_", n_sts, "/")
  dir.create(dir_name, FALSE, TRUE)
  
  # New proposal matrix
  new_proposal_matrix <- as.matrix(read.csv(paste0(dir_name, "new_proposal_mtx.csv")))
  new_proposal_matrix <- apply(new_proposal_matrix, 2, as.numeric)
  new_proposal_matrix <- new_proposal_matrix #*10 # 100 resulted in bad chains while lower denominators resulted in jumpy steps among chains
  new_proposal_matrix <- (new_proposal_matrix + t(new_proposal_matrix)) / 2
  rownames(new_proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta", "alpha", "gamma_annual", "nu_annual", "kappa_Ne", "kappa_12F", "kappa_55")
  colnames(new_proposal_matrix) <- c("log_A_ini", "time_shift_1", "time_shift_2", "beta_0", "beta_1", "beta_2", "scaled_wane", "log_delta", "alpha", "gamma_annual", "nu_annual", "kappa_Ne", "kappa_12F", "kappa_55")
  # isSymmetric(new_proposal_matrix)
  
  tune_mcmc_pars <- prepare_parameters(initial_pars = pars, priors = priors, proposal = new_proposal_matrix, transform = transform)
  
  # Including adaptive proposal control
  # https://mrc-ide.github.io/mcstate/reference/adaptive_proposal_control.html
  tune_control <- mcstate::pmcmc_control(n_steps = n_sts,
                                         n_chains = 4,
                                         rerun_every = 50,
                                         rerun_random = TRUE,
                                         progress = TRUE
  )
  
  filter <- mcstate::particle_filter$new(data = sir_data,
                                         model = gen_sir, # Use odin.dust input
                                         n_particles = n_pars,
                                         compare = case_compare,
                                         seed = 1L
  )
  
  # The pmcmc
  tune_pmcmc_result <- mcstate::pmcmc(tune_mcmc_pars, filter_deterministic, control = tune_control)
  tune_pmcmc_result
  saveRDS(tune_pmcmc_result, paste0(dir_name, "tune_pmcmc_result.rds"))
  
  new_proposal_mtx <- cov(pmcmc_result$pars)
  write.csv(new_proposal_mtx, paste0(dir_name, "new_proposal_mtx.csv"), row.names = FALSE)
  
  tune_lpost_max <- which.max(tune_pmcmc_result$probabilities[, "log_posterior"])
  write.csv(as.list(tune_pmcmc_result$pars[tune_lpost_max, ]),
            paste0(dir_name, "tune_initial.csv"), row.names = FALSE)
  
  # Further processing for thinning chains
  mcmc2 <- coda::as.mcmc(cbind(
    tune_pmcmc_result$probabilities, tune_pmcmc_result$pars))
  write.csv(mcmc2, paste0(dir_name, "mcmc2.csv"), row.names = FALSE)
  
  mcmc2_burnedin <- tuning_pmcmc_further_process(n_sts, tune_pmcmc_result)
  write.csv(mcmc2_burnedin, paste0(dir_name, "mcmc2_burnedin.csv"), row.names = FALSE)
  
  # Calculating ESS & Acceptance Rate
  tune_calc_ess <- ess_calculation(mcmc2)
  write.csv(tune_calc_ess, paste0(dir_name, "tune_calc_ess.csv"), row.names = FALSE)
  
  tune_calc_ess_burnedin <- ess_calculation(mcmc2_burnedin)
  write.csv(tune_calc_ess_burnedin, paste0(dir_name, "tune_calc_ess_burnedin.csv"), row.names = FALSE)
  
  # Figures! (still failed, margin error)
  fig <- pmcmc_trace(mcmc2)
  fig <- pmcmc_trace(mcmc2_burnedin)
  
  ##############################################################################
  # MCMC Diagnostics
  
  # 1. Gelman-Rubin Diagnostic
  # https://cran.r-project.org/web/packages/coda/coda.pdf
  figs_gelman_init <- diag_init_gelman_rubin(tune_pmcmc_result)
  fig <- diag_cov_mtx(figs_gelman_init)
  fig <- diag_gelman_rubin(figs_gelman_init)
  
  # 2. Autocorrelation
  fig <- diag_aucorr(mcmc2)
  
  # 3. ggpairs
  fig <- GGally::ggpairs(as.data.frame(tune_pmcmc_result$pars))
  
}

pmcmc_run2 <- pmcmc_run2(design=Design, replications=1000, generate=Generate,
                     analyse=Analyse, summarise=Summarise,
                     parallel = 'future')

# a slight modification for college's HPC
args <- commandArgs(trailingOnly = T)
n_pars <- as.numeric(args[which(args == "--n_particles") + 1])
n_sts <- as.numeric(args[which(args == "--n_steps") + 1])

pmcmc_run2(n_pars, n_sts)
