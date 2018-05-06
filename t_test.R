library(tidyverse)
library(magrittr)
source("plot_functions.R")

#Implementation of agnostic t-test
agnostic_t_test <- function(x, alternative = "equal", 
                            mu0 = 0, alpha = 0.05, beta = 0.05) {
  if(!(alternative %in% c("equal", "lesser", "greater")))
    stop("Alternative parameter must be one of 'equal','lesser' or 'greater'")
  decision = NA
  
  lower_sign = ifelse(alternative == "equal", 0.5*alpha, alpha)
  upper_sign = ifelse(alternative == "equal", 0.5*(1-beta), 1-beta)
  t_stat = ((mean(x) - mu0)/(sd(x)/sqrt(length(x))))
  stat = ifelse(alternative == "equal", abs(t_stat), t_stat)
  
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

#Simulation of bilateral t-test
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
    decision[i,b] = agnostic_t_test(rnorm(n, mean = mu.grid[i], 2))$decision
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
  mutate(Power = ifelse(mu.grid == 0, `Accept H0`, `Reject H0`))

#Save results
write_rds(prob_decisions, "./data/t_test_equal.rds")
prob_decisions = read_rds("./data/t_test_equal.rds")

#Figures
#Decision probabilities
decision_plot(prob_decisions, "./figures/t_test_equal_1.eps")

#Power function
power_plot(prob_decisions, "./figures/t_test_equal_2.eps")

#Simulation of one-sided agnostic t-test
n = 10
mu.grid = seq(-2, 2, length.out = 100)
mu.grid = sort(c(mu.grid,0))
B = 10^5
decision = matrix(NA, length(mu.grid), B)
for(i in 1:length(mu.grid))
{
  print(i/length(mu.grid))
  for(b in 1:B)  
  {
    decision[i,b] = agnostic_t_test(rnorm(n, mean = mu.grid[i], 2), 
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
write_rds(prob_decisions, "./data/t_test_lesser.rds")
prob_decisions = read_rds("./data/t_test_lesser.rds")

#Figures
#Decision probabilities
decision_plot(prob_decisions, "./figures/t_test_lesser_1.eps")

#Power function
power_plot(prob_decisions, "./figures/t_test_lesser_2.eps")
