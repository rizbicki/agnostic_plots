agnostic.oneSample <- function(x,sd, alternative = "two.sided", mu0 = 0, alpha = 0.05, beta = 0.05) {
  t = ((mean(x) - mu0)/(sd/sqrt(length(x))))
  if(alternative == "two.sided") {
    c0 <- qnorm(alpha, lower.tail = FALSE)
    c1 <- qnorm((1-beta),  lower.tail = FALSE)
    if(abs(t) >= c0) 
      decision <- "Reject H0"
    else if(abs(t) <= c1)
      decision <- "Accept H0"
    else
      decision <- "Agnostic"
    return(list(decision = decision, limits = c(c1,c0), abs.test.statistic = abs(t)))
  } else if(alternative == "less") {
    c0 <- qnorm(alpha,  lower.tail = FALSE)
    c1 <- qnorm(1-beta,  lower.tail = FALSE)
    if(t >= c0) 
      decision <- "Reject H0"
    else if(t <= c1)
      decision <- "Accept H0"
    else
      decision <- "Agnostic"
    return(list(decision = decision, limits = c(c1,c0), test.statistic = t))
  } else if(alternative == "greater") {
    c0 <- qnorm(alpha, lower.tail = FALSE)
    c1 <- qnorm(1-beta,  lower.tail = FALSE)
    if(t >= c0) 
      decision <- "Accept H0"
    else if(t <= c1)
      decision <- "Reject H0"
    else
      decision <- "Agnostic"
    return(list(decision = decision, limits = c(c1,c0), test.statistic = t))
  } else
    stop("invalid alternative parameter, use one of 'two.sided','less' or 'greater'")
}


n=10
mu.grid=seq(-2,2,length.out = 100)
mu.grid=sort(c(mu.grid,0))
B=100000
decision=matrix(NA,length(mu.grid),B)
for(i in 1:length(mu.grid))
{
  print(i/length(mu.grid))
  for(b in 1:B)  
  {
    decision[i,b]=agnostic.oneSample(rnorm(n,mean = mu.grid[i],2),sd = 2,
                                     alternative = "less")$decision
  }
}

decisions.names=c("Agnostic","Reject H0","Accept H0")
probability.decisions=t(apply(decision,1,function(x)
{
  return(apply(as.matrix(decisions.names),1,function(y)
  {
    mean(x==y)
  }))
}))
colnames(probability.decisions)=decisions.names
probability.decisions=cbind(mu.grid,as.data.frame(probability.decisions))


power1=apply(decision[mu.grid==0,],1,function(x)
{
  mean(x=="Accept H0")
})
power2=apply(decision[mu.grid!=0,],1,function(x)
{
  mean(x=="Reject H0")
})

library(tidyr)
probability.decisions=gather(probability.decisions,"Decision","Probability",Agnostic:`Accept H0`)


library(ggplot2)
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top", legend.title=element_blank(),panel.grid.major.x=element_blank())

pdf("one_sample.pdf")

ggplot()+geom_line(data=probability.decisions,aes(x=mu.grid,y=Probability,linetype=Decision,color=Decision),size=2)+
  geom_hline(yintercept = 0.05,linetype=2,color="grey20")+xlab(expression(mu))+geom_vline(xintercept = 0)+ylab("Probability of each decision")+
  geom_hline(yintercept = 0.0)+ylim(c(0,1))


power.data=as.data.frame(cbind(mu.grid,c(power1,power2)))
names(power.data)=c("mu.grid","Power")

ggplot()+geom_line(data=power.data,aes(x=mu.grid,y=Power),size=2)+
  geom_hline(yintercept = 0.05,linetype=2,color="grey20")+xlab(expression(mu))+
  geom_vline(xintercept = 0)+ylab(expression(pi(mu)))+
  geom_hline(yintercept = 0.0)+ylim(c(0,1))

dev.off()


pdf("one_sample.pdf")

ggplot()+geom_line(data=probability.decisions,aes(x=mu.grid,y=Probability,linetype=Decision,color=Decision),size=2)+
  geom_hline(yintercept = 0.05,linetype=2,color="grey20")+xlab(expression(mu))+geom_vline(xintercept = 0)+ylab("Probability of each decision")+
  geom_hline(yintercept = 0.0)+ylim(c(0,1))


power.data=as.data.frame(cbind(mu.grid,c(power1,power2)))
names(power.data)=c("mu.grid","Power")

ggplot()+geom_line(data=power.data,aes(x=mu.grid,y=Power),size=2)+
  geom_hline(yintercept = 0.05,linetype=2,color="grey20")+xlab(expression(mu))+
  geom_vline(xintercept = 0)+ylab(expression(pi(mu)))+
  geom_hline(yintercept = 0.0)+ylim(c(0,1))

dev.off()
