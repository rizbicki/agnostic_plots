library(tidyverse)
library(magrittr)
source("plot_functions.R")

#Implementation of agnostic z-test
agnostic_z_test <- function(x, sd, alternative = "equal", 
                            mu0 = 0, alpha = 0.05, beta = 0.05) {
  if(!(alternative %in% c("equal", "lesser", "greater")))
    stop("Alternative parameter must be one of 'equal','lesser' or 'greater'")
  decision = NA
  
  lower_sign = ifelse(alternative == "equal", 0.5*alpha, alpha)
  upper_sign = ifelse(alternative == "equal", 0.5*(1-beta), 1-beta)
  z_stat = ((mean(x) - mu0)/(sd/sqrt(length(x))))
  stat = ifelse(alternative == "equal", abs(z_stat), z_stat)
  
  # test constants
  c0 = qnorm(lower_sign, lower.tail = FALSE)
  c1 = qnorm(upper_sign,  lower.tail = FALSE)
  if(stat >= c0) 
    decision = 1
  else if(stat <= c1) 
    decision = 0
  else 
    decision = 0.5
  if(alternative == "greater") 
    decision = (1 - decision)
  return(list(decision = decision, limits = c(c1,c0), test.statistic = stat))
}

#Simulation of one-sided agnostic z-test
n = 10
mu.grid = seq(-2, 2, length.out = 100)
mu.grid = sort(c(mu.grid, 0))
B = 10^5
decision = matrix(NA, length(mu.grid), B)
for(i in 1:length(mu.grid))
{
  print(i/length(mu.grid))
  for(b in 1:B)
  {
    decision[i, b] = agnostic_z_test(rnorm(n, mean = mu.grid[i], 2), sd = 2,
                                     alternative = "lesser")$decision
  }
}

#Power analysis
possible_decisions = as.matrix(c(0, 0.5, 1))
prob_of_each_decision = function(x) {
  apply(possible_decisions, 1, function(decisions) mean(x == decisions)) }
prob_decisions = t(apply(decision, 1, prob_of_each_decision))
prob_decisions %<>%
  cbind(mu.grid, .) %>%
  as.tibble() %>%
  select(mu.grid, `Accept H0` = V1, `Agnostic` = V2, `Reject H0` = V3) %>%
  mutate(Power = ifelse(mu.grid <= 0, `Accept H0`, `Reject H0`))

#Save results
#write_rds(prob_decisions, "./data/z_test_lesser.rds")
#prob_decisions = read_rds("./data/z_test_lesser.rds")

#Decision probabilities
decision_plot(prob_decisions, "./figures/one_sample_1.eps")

#Power function
power_plot(prob_decisions, "./figures/one_sample_2.eps")
