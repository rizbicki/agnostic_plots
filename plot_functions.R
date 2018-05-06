#Figures
#General
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position = "top",
                     legend.title = element_blank(),
                     panel.grid.major.x = element_blank())

decision_plot <- function(prob_decisions)
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
}

power_plot <- function(prob_decisions, discontinuity = c())
{
  powers = prob_decisions %>%
    select(mu.grid, Power)
  disc_pos = which(mu.grid %in% discontinuity)
  discontinuous_power = powers[disc_pos, ]
  continuous_power = data_frame(mu.grid = powers$mu.grid[disc_pos],
                                Power = powers$Power[disc_pos-1])
  
  powers %>%
    filter(!(mu.grid %in% discontinuity)) %>%
    ggplot() +
    geom_line(aes(x = mu.grid, y = Power), size = 2) +
    geom_hline(yintercept = 0.05 , linetype = 2 , color="grey20") +
    xlab(expression(mu)) +
    geom_vline(xintercept = 0) +
    ylab(expression(pi(mu))) +
    geom_hline(yintercept = 0.0) +
    ylim(c(0,1)) +
    geom_point(aes(x = mu.grid, y = Power), size = 3, data = discontinuous_power) +
    geom_point(aes(x = mu.grid, y = Power), size = 3, pch = 1 , data = continuous_power)
}

plot_interval_test <- function(eps = 0.2, fac = 3)
{
  plot(c(0,10), c(1,8), pch = NA, 
       xaxt = 'n', yaxt = 'n', 
       bty = 'n',
       xlab = '', ylab = '')
  
  y=2
  lines(c(-10, 8), c(y-eps, y-eps), lwd = 0.3)
  lines(c(1.2, 5), c(y, y), lwd = 3)
  lines(c(3, 3.8), c(y+eps, y+eps), lwd = 3, col = 2, lty = 3)
  lines(c(3, 3.8), c(y+eps, y+eps), lwd = 1, col = 2, lty = 1)
  lines(c(3.3, 3.3), c(y-fac*eps, 6+fac*eps), lwd = 2)
  text(3.3, y-(fac+1.5)*eps, expression(theta[0]))
  text(9, y-eps, substitute(paste("Accept ", H[0])))
  text(0.3, y, substitute(paste("Sample 3")))
  
  y=4
  lines(c(-10, 8), c(y-eps, y-eps), lwd = 0.3)
  lines(c(1.2, 5), c(y, y), lwd = 3)
  lines(c(3.9, 4.8), c(y+eps, y+eps), lwd = 3.2, col = 2, lty = 3)
  lines(c(3.9, 4.8), c(y+eps, y+eps), lwd = 1, col = 2, lty = 1)
  text(9, y-eps, substitute(paste("Agnostic about ", H[0])))
  text(0.3, y, substitute(paste("Sample 2")))
  
  y=6
  lines(c(-10, 8), c(y-eps, y-eps), lwd = 0.3)
  lines(c(1.2+2.5, 5+2.5), c(y, y), lwd = 3)
  lines(c(3+3, 3.8+3), c(y+eps, y+eps), lwd = 3.2, col = 2, lty = 3)
  lines(c(3+3, 3.8+3), c(y+eps, y+eps), lwd = 1, col = 2, lty = 1)
  text(9, y-eps,substitute(paste("Reject ", H[0])))
  text(0.3, y,substitute(paste("Sample 1")))
  
  legend("topright", c(expression(R[1]), expression(R[2])),
         col = c(2,1), lwd = 3, lty = c(3,1),
         bty = "n",title = "Confidence Interval")
  legend("topright", c(expression(R[1]), expression(R[2])),
         col = c(2,1) , lwd = 1, lty = c(1,1),
         bty="n", title = "Confidence Interval")
}
