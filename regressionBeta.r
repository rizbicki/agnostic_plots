#zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
#  if (length(x) == 1) return(TRUE)
#  x <- range(x) / mean(x)
#  isTRUE(all.equal(x[1], x[2], tolerance = tol))
#}

library(tidyr)
library(dplyr)
library(ggplot2)
theme = theme_set(theme_minimal(base_size = 20))
theme = theme_update(legend.position="top", legend.key.width=unit(2.2,"cm"),
                     legend.title=element_blank(), 
                     panel.grid.major.x=element_blank(),
                     panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.border = element_rect(colour = "black", 
                                                 fill=NA, size=0.5),
                     axis.text.y = element_text(colour="black",size=13), 
                     axis.text.x = element_text(size=13),
                     axis.ticks.y= element_line(colour="black"))+ 
  theme_update(axis.ticks.length=unit(.15, "cm"),
               panel.spacing.y = unit(1.5, "lines"))

find.c.0=function(d,df,element.inverse,beta){
  c.0=seq(0,10,0.005)
  beta.true=rep(NA,length(c.0))
  for(i in 1:length(c.0))
  {
    beta.true[i]=suppressWarnings(pt(c.0[i],df,(1/sqrt(element.inverse))*d))-
      suppressWarnings(pt(-c.0[i],df,(1/sqrt(element.inverse))*d))
  }
  return(c.0[FNN::get.knnx(beta.true,beta,k=1)$nn.index])
}

lm.agnostic=function(formula,data,d=NULL,beta=0.05,
                     alpha=0.05,plot.power=FALSE,
                     scale=TRUE)
{
  if(!is.null(d))
  {
    if(scale)
    {
      which.numeric=sapply(data,is.numeric)
      data.scale=data
      data.scale[,which.numeric]=scale(data[,which.numeric])
      fit <- lm(formula, data = as.data.frame(data.scale))
    } else {
      fit <- lm(formula, data = data)
    }
    elements.inverse=diag(vcov(fit)/summary(fit)$sigma^2) # elements of solve(t(X)%*%X)
    if(length(d)==1)
      d=rep(d,length(elements.inverse))
    c.0=rep(NA,length(d))
    for(i in 1:length(d))
      c.0[i]=find.c.0(d[i],fit$df.residual,elements.inverse[i],beta)
    c.1=qt(1-alpha/2,fit$df.residual)
    if(any(c.0>c.1))
      stop("c.0>c.1: either decrease beta, decrease d, or decrease alpha")
    statistic=abs(summary(fit)$coefficients[,3])
    Decision=rep("Accept",length(statistic))
    Decision[statistic>c.0]="Agnostic"
    Decision[statistic>c.1]="Reject"
    
    if(plot.power)
    {
      d.grid=seq(0,1,length.out = 1000)
      prob.accept=prob.reject=prob.agnostic=matrix(NA,length(d.grid),length(elements.inverse))
      colnames(prob.accept)=names(elements.inverse)
      colnames(prob.reject)=names(elements.inverse)
      colnames(prob.agnostic)=names(elements.inverse)
      for(i in 1:length(elements.inverse))
      {
        for(j in 1:length(d.grid))
        {
          prob.accept[j,i]=suppressWarnings(pt(c.0[i],fit$df.residual,
                                               (1/sqrt(elements.inverse[i]))*d.grid[j]))-
            suppressWarnings(pt(-c.0[i],fit$df.residual,
                                (1/sqrt(elements.inverse[i]))*d.grid[j]))
          prob.reject[j,i]=suppressWarnings(pt(c.1,fit$df.residual,
                                               (1/sqrt(elements.inverse[i]))*d.grid[j],lower.tail = FALSE))+
            suppressWarnings(pt(-c.1,fit$df.residual,
                                (1/sqrt(elements.inverse[i]))*d.grid[j]))
          prob.agnostic[j,i]=1-prob.reject[j,i]-prob.accept[j,i]
        }
      }
      prob.accept=cbind("Accept",d.grid,gather(as.data.frame(prob.accept),coefficient,prob,1:ncol(prob.accept)))
      prob.reject=cbind("Reject",d.grid,gather(as.data.frame(prob.reject),coefficient,prob,1:ncol(prob.reject)))
      prob.agnostic=cbind("Agnostic",d.grid,gather(as.data.frame(prob.agnostic),coefficient,prob,1:ncol(prob.agnostic)))
      names(prob.accept)[1]=names(prob.reject)[1]=names(prob.agnostic)[1]="Decision"
      probs=rbind(prob.accept,prob.reject,prob.agnostic)
      g=ggplot(data=probs ,aes(x=d.grid, y=prob, group=Decision)) +
        geom_line(aes(linetype=Decision, color=Decision),size=1.5)+
        facet_wrap( ~ coefficient, 
                    ncol=round(sqrt(length(elements.inverse))))+
        ylab("Probability of the Decision")+
        xlab("Cohen's d effect size")+ 
        geom_hline(yintercept=beta,color="black")+ 
        annotate("text", min(prob.accept$d.grid), beta+0.05,size=7, 
                 label = "beta",parse=TRUE,color="black")+
        geom_hline(yintercept=alpha,color="black")+ 
        annotate("text", min(prob.accept$d.grid), alpha+0.05, size=7,
                 label = "alpha",parse=TRUE,color="black")
      print(g)
      
    }
    if(scale)
    {
      fit <- lm(formula, data = data)
    }
    return(list(summary=cbind(summary(fit)$coefficients,Decision),
                c.0=c.0,
                c.1=c.1))
  }
}

data=data.frame(x1=rnorm(100),x2=rnorm(100),x3=rbeta(100,20,1),x4=rbeta(100,1,1),x5=rnorm(100),x6=rnorm(100,10,10))
data$y=2*data$x1+0.05*data$x2+0.2*data$x3+rnorm(100,0,2)
lm.agnostic("y~.",data,d=0.2,beta=0.2,alpha=0.05,plot.power = T)

aux=lm.agnostic("Infant.Mortality~.",swiss,d=0.2,beta=0.05,alpha=0.05,plot.power = T)
aux$summary[,1:4]=round(as.numeric(aux$summary[,1:4]),3)
xtable::xtable(aux$summary)

aux=lm.agnostic("Infant.Mortality~.",swiss,d=0.25,beta=0.2,alpha=0.05,plot.power = T)
aux$summary[,1:4]=round(as.numeric(aux$summary[,1:4]),3)
xtable::xtable(aux$summary)


pdf("infant_power.pdf")
aux=lm.agnostic("Infant.Mortality~.",swiss,d=0.25,beta=0.2,alpha=0.05,plot.power = T)
dev.off()
aux$summary[,1:4]=round(as.numeric(aux$summary[,1:4]),3)
xtable::xtable(aux$summary)

aux=lm.agnostic("Infant.Mortality~.",swiss,d=0,beta=0.05,alpha=0.05,plot.power = T)
aux$summary[,1:4]=round(as.numeric(aux$summary[,1:4]),3)
xtable::xtable(aux$summary)


lm.agnostic("price~.",Ecdat::Housing,d=0.1,beta=0.05,alpha=0.05,plot.power = T)
lm.agnostic("duration~.",Ecdat::Unemployment,d=0.1,beta=0.05,alpha=0.05,plot.power = T)
lm.agnostic("wage~.",Ecdat::Wages1,d=0.05,beta=0.005,alpha=0.005,plot.power = T)
head(Ecdat::Wages1)
