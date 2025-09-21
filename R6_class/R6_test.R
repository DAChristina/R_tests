source("R6_class/R6_script.R")
library(tidyverse)
define <- randomFun$new()
# define$obj1 <- 100
define$initialise(obj1 = 100)

define$runBurnin()
# results <- define$runjMonths(j = 12)
define$runjMonths(j = 12)
define$save("R6_class/simulation_outputs.rds")

result <- readRDS("R6_class/simulation_outputs.rds")
result_df <- data.frame(
  result2 = result$result2,
  result3 = result$result3
  ) #%>% 
  # dplyr::mutate(time = seq(1, n()))




