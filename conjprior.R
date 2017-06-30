## Relating a Gaussian Distribution with (mean,variance) to a Beta Distribution with parameters (alpha,beta)

Beta_Parameters <- function(mean, variance) {
  alpha <- ((1 - mean) / variance - 1 / mean) * mean ^ 2
  beta <- alpha * (1 / mean - 1)
  return(Beta_Parameters = list(alpha = alpha, beta = beta))
}

# Assign values to the Gaussian parameters
mean<-0
variance<-1

par <-Beta_Parameters(mean,variance)

# Return a plot of the original Gaussian and of the approximated Beta Distribution

x=seq(-10,10,length=10000)
y1=dnorm(x,mean,variance)
y2=dbeta(x,par$alpha,par$beta)

plot(x,y1, type="l", col="black", lwd=2, ylab='P(x)',xlab= expression(x))

lines(x,y2, col="orange",lwd=2)
legend(0.1,18,legend = c("Gaussian", "Beta"),col=c("black","orange"), lwd=5, cex=1, horiz = TRUE)
