set.seed(1000)
xseq<-seq(-5,5,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1)

plot(xseq, densities*100, col="darkgreen",
     xlab="", ylab="Percentage", 
     type="l",lwd=2, cex=2, ylim=c(0, 100))

lines(xseq, cumulative*100, col="darkorange", 
     xlab="", type="l",lwd=2, cex=2)