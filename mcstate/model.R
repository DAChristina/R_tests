freq <- user(1) # model is daily but aggregated to weekly
dt <- 1/freq
initial(time) <- 0

# 1. PARAMETERS ################################################################
N <- user(6.7e7) # FIXED England's pop size is roughly 67,000,000
log_A_ini <- user(0) # S_ini*10^(log10(-5.69897)) = 120 people; change A_ini into log10(A_ini)
D_ini <- user(0) 
time_shift_1 <- user(0)
time_shift_2 <- user(0)
beta_0 <- user(0)
beta_1 <- user(0)
beta_2 <- user(0)

max_wane <- user(-5) # FIXED, scaled waning immunity
min_wane <- user(-10) # FIXED, scaled waning immunity
scaled_wane <- user(0)

# No vaccination effect for 12F
# Country calibration:
# Children: 1.07638532472038 (it is called delta in the spreadsheet)
# Adults: 0.536936186788821 (basically gamma in the spreadsheet)
# Average: 0.8066608
UK_calibration <- user(0.8066608) # FIXED (Lochen et al., 2022)

log_delta <- user(0) # required in mcState
hypo_sigma_day <- user(28) # 28 days
sigma_1 <- 1/hypo_sigma_day
sigma_2 <- user(0)
mu_0 <- 1/(80.70*365) # background mortality per day, the inverse of life expectancy
mu_1 <- user(0)
pi <- user(3.141593) # FIXED

# 2. INITIAL VALUES ############################################################
A_ini <- 10^(log_A_ini)*N
initial(A) <- A_ini
initial(D) <- D_ini
initial(S) <- N - A_ini - D_ini
initial(R) <- 0
initial(n_AD_weekly) <- 0 #infections
# initial(n_AD_cumul) <- 0

# 3. UPDATES ###################################################################
beta <- beta_0*(
  (1+beta_1*cos(2*pi*((time_shift_1*365)+time)/365)) + 
    (1+beta_2*sin(2*pi*((time_shift_2*365)+time)/365)))

# lambda <- beta*(A+D)/N
lambda <- if ((A+D) > 0) beta*(A+D)/N else 0
delta <- (10^(log_delta))*UK_calibration

log_wane <- scaled_wane*(max_wane-min_wane)+min_wane # scaled_wane*(max_wane−min_wane)+min_wane; rescaled using (wane-wane_min)/(wane_max-wane_min)
wane <- 10^(log_wane)
# wane <- 0

# Individual probabilities of transition
p_SA <- 1- exp(-(lambda+mu_0) * dt)
p_Asym <- 1- exp(-(delta+mu_0+sigma_1) * dt)
p_Dis <- 1- exp(-(sigma_2+mu_0+mu_1) * dt)
p_RS <- 1- exp(-(wane+mu_0) * dt)

# Draws for numbers changing between compartments
# Leaving S
n_Suscep <- rbinom(S, p_SA)
n_SA <- rbinom(n_Suscep, lambda/(lambda+mu_0))
n_S_dead <- n_Suscep - n_SA

# Leaving A
n_Asym <- rbinom(A, p_Asym) # n_Asym <- n_AD + n_AR cause cyclic dependency error
n_AD <- rbinom(n_Asym, delta/(delta+mu_0+sigma_1))
n_AR <- rbinom((n_Asym - n_AD), sigma_1/(mu_0+sigma_1))
n_A_dead <- n_Asym - n_AD - n_AR

# Leaving D
n_Dis <- rbinom(D, p_Dis)
n_DR <- rbinom(n_Dis, sigma_2/(sigma_2+mu_0+mu_1))
n_Dd <- rbinom((n_Dis - n_DR), mu_1/(mu_0+mu_1))
n_D_dead <- n_Dis - n_DR - n_Dd

# Leaving R
n_Resist <- rbinom(R, p_RS)
n_RS <- rbinom(n_Resist, wane)
n_R_dead <- n_Resist - n_RS

# Closed system: births = deaths; all born susceptible
n_S_born <- n_S_dead + n_Dd + n_D_dead + n_A_dead + n_R_dead

# The transitions
update(time) <- (step + 1) * dt
update(S) <- S - n_SA + n_RS + n_S_born - n_S_dead
update(A) <- A + n_SA - (n_AD + n_AR) - n_A_dead
update(D) <- D + n_AD - (n_DR + n_Dd) - n_D_dead
update(R) <- R + n_AR + n_DR - n_RS - n_R_dead
# that "little trick" previously explained in https://github.com/mrc-ide/dust/blob/master/src/sir.cpp for cumulative incidence:
# based on tutorial: https://mrc-ide.github.io/odin-dust-tutorial/mcstate.html#/the-model
update(n_AD_weekly) <- if (step %% 7 == 0) n_AD else n_AD_weekly + n_AD
# update(n_AD_cumul) <- n_AD_cumul + n_AD # no interest in asymptomatic cases that've recovered

