#Figures
#General
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position = "top",
                     legend.title = element_blank(),
                     panel.grid.major.x = element_blank())

decision_plot <- function(prob_decisions, path, this_device = "eps")
{
  prob_decisions %>%
    select(-Power) %>%
    gather("Decision", "Probability", -mu.grid) %>%
    ggplot() +
    geom_line(
      aes(x = mu.grid, y = Probability, linetype = Decision, color = Decision), size = 2) +
    geom_hline(yintercept = 0.05, linetype=2, color="grey20") + 
    xlab(expression(mu)) +
    geom_vline(xintercept = 0) +
    ylab("Probability of each decision") +
    geom_hline(yintercept = 0.0) +
    ylim(c(0,1))
  ggsave(path, device = this_device)
}

power_plot <- function(prob_decisions, path, this_device = "eps")
{
  prob_decisions %>%
    select(mu.grid, Power) %>%
    ggplot() +
    geom_line(aes(x = mu.grid, y = Power), size = 2) +
    geom_hline(yintercept = 0.05 , linetype=2 , color="grey20") +
    xlab(expression(mu)) +
    geom_vline(xintercept = 0) +
    ylab(expression(pi(mu))) +
    geom_hline(yintercept = 0.0) +
    ylim(c(0,1))
  ggsave(path, device = this_device) 
}
