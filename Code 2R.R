#Simulating a white noise process

z_t <- rnorm(200,0,1)
plot(z_t,xlab = "t",ylab = expression(z[t]),type = "l",main = "White Noise")

#Simulating ARIMA models

ts.sim<-arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))
ts.plot(ts.sim)
# mildly long-tailed
ts.sim<-arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))
plot(ts.sim)
# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
ts.plot(ts.sim)

#Simulating random walks: X_t = x_0 + t*mu + z_t

RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}

P1<-RW(100,10,0,0.0004)
P2<-RW(100,10,0,0.0004)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="blue")
