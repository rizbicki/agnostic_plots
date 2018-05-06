pdf("interval_tTest.pdf")
eps=0.2
fac=3
plot(c(0,10),c(1,8),pch=NA,xaxt='n',yaxt='n',bty='n',
     ylab='',xlab='')

y=2
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2,5),c(y,y),lwd=3)
lines(c(3,3.8),c(y+eps,y+eps),lwd=3,col=2,lty=3)
lines(c(3,3.8),c(y+eps,y+eps),lwd=1,col=2,lty=1)
lines(c(3.3,3.3),c(y-fac*eps,6+fac*eps),lwd=2)
text(3.3,y-(fac+1.5)*eps,expression(theta[0]))
text(9,y-eps,substitute(paste("Accept ",H[0])))
text(0.3,y,substitute(paste("Sample 3")))

y=4
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2,5),c(y,y),lwd=3)
lines(c(3.9,4.8),c(y+eps,y+eps),lwd=3.2,col=2,lty=3)
lines(c(3.9,4.8),c(y+eps,y+eps),lwd=1,col=2,lty=1)
#lines(c(3.3,3.3),c(y-fac*eps,y+fac*eps),lwd=2)
#text(3.3,y-(fac+1.5)*eps,expression(mu[0]))
text(9,y-eps,substitute(paste("Agnostic about ",H[0])))
text(0.3,y,substitute(paste("Sample 2")))

y=6
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2+2.5,5+2.5),c(y,y),lwd=3)
lines(c(3+3,3.8+3),c(y+eps,y+eps),lwd=3.2,col=2,lty=3)
lines(c(3+3,3.8+3),c(y+eps,y+eps),lwd=1,col=2,lty=1)
#lines(c(3.3,3.3),c(y-fac*eps,y+fac*eps),lwd=2)
#text(3.3,y-(fac+1.5)*eps,expression(mu[0]))
text(9,y-eps,substitute(paste("Reject ",H[0])))
text(0.3,y,substitute(paste("Sample 1")))


legend("topright",c(expression(R[1]),expression(R[2])),
       col=c(2,1),lwd=3,lty=c(3,1),bty="n",title = "Confidence Interval")
legend("topright",c(expression(R[1]),expression(R[2])),
       col=c(2,1),lwd=1,lty=c(1,1),bty="n",title = "Confidence Interval")
dev.off()



setEPS()
postscript("interval_tTest.eps")
eps=0.2
fac=3
plot(c(0,10),c(1,8),pch=NA,xaxt='n',yaxt='n',bty='n',
     ylab='',xlab='')

y=2
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2,5),c(y,y),lwd=3)
lines(c(3,3.8),c(y+eps,y+eps),lwd=3,col=2,lty=3)
lines(c(3,3.8),c(y+eps,y+eps),lwd=1,col=2,lty=1)
lines(c(3.3,3.3),c(y-fac*eps,6+fac*eps),lwd=2)
text(3.3,y-(fac+1.5)*eps,expression(theta[0]))
text(9,y-eps,substitute(paste("Accept ",H[0])))
text(0.3,y,substitute(paste("Sample 3")))

y=4
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2,5),c(y,y),lwd=3)
lines(c(3.9,4.8),c(y+eps,y+eps),lwd=3.2,col=2,lty=3)
lines(c(3.9,4.8),c(y+eps,y+eps),lwd=1,col=2,lty=1)
#lines(c(3.3,3.3),c(y-fac*eps,y+fac*eps),lwd=2)
#text(3.3,y-(fac+1.5)*eps,expression(mu[0]))
text(9,y-eps,substitute(paste("Agnostic about ",H[0])))
text(0.3,y,substitute(paste("Sample 2")))

y=6
lines(c(-10,8),c(y-eps,y-eps),lwd=0.3)
lines(c(1.2+2.5,5+2.5),c(y,y),lwd=3)
lines(c(3+3,3.8+3),c(y+eps,y+eps),lwd=3.2,col=2,lty=3)
lines(c(3+3,3.8+3),c(y+eps,y+eps),lwd=1,col=2,lty=1)
#lines(c(3.3,3.3),c(y-fac*eps,y+fac*eps),lwd=2)
#text(3.3,y-(fac+1.5)*eps,expression(mu[0]))
text(9,y-eps,substitute(paste("Reject ",H[0])))
text(0.3,y,substitute(paste("Sample 1")))


legend("topright",c(expression(R[1]),expression(R[2])),
       col=c(2,1),lwd=3,lty=c(3,1),bty="n",title = "Confidence Interval")
legend("topright",c(expression(R[1]),expression(R[2])),
       col=c(2,1),lwd=1,lty=c(1,1),bty="n",title = "Confidence Interval")
dev.off()
