source("plot_functions.R")

pdf("./figures/interval_t_test.pdf")
plot_interval_test()
dev.off()

setEPS()
postscript("./figures/interval_t_test.eps")
plot_interval_test()
dev.off()
