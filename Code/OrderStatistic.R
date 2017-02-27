library(ggplot2)
#Generate normal distribution
set.seed(100)
N <- 1000000
samp <- as.data.frame(rnorm(N))
colnames(samp) <- "value"
ggplot(samp,aes(samp)) + geom_histogram(bins = 500, aes(y=..count../max(..count..))) + labs(x ="x",y = "")
#Take L samples of size n from the normal distribution
n <- 10
L <- 5000
mn <- array(dim = L)
mn2 <- array(dim = L)
mx <- array(dim = L)
for (i in 1:L) {
  s <- sample(samp$value,n)
  mn[i] <- min(s)
  mn2[i] <- sort(s)[2]
  mx[i] <- max(s)
}
mn <- as.data.frame(mn)
mn2 <- as.data.frame(mn2)
mx <- as.data.frame(mx)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Minimum Value", y= "")
mean(mn$mn) 
ggplot(mn2,aes(x = mn2)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "2nd Smallest Value", y= "")
mean(mn2$mn2)
ggplot(mx,aes(x = mx)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Maximum Value", y= "")
mean(mx$mx)
#Repeat for samples of size 20
n <- 20
L <- 500
mn <- array(dim = L)
mn2 <- array(dim = L)
mx <- array(dim = L)
for (i in 1:L) {
  s <- sample(samp$value,n)
  mn[i] <- min(s)
  mn2[i] <- sort(s)[2]
  mx[i] <- max(s)
}
mn <- as.data.frame(mn)
mn2 <- as.data.frame(mn2)
mx <- as.data.frame(mx)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Minimum Value", y= "")
mean(mn$mn) 
ggplot(mn2,aes(x = mn2)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "2nd Smallest Value", y= "")
mean(mn2$mn2)
ggplot(mx,aes(x = mx)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Maximum Value", y= "")
mean(mx$mx)

#Now try to compute the value directly
fCDF <- function(f){
  function(x){
    integrate(f,-Inf,x)[[1]]
  }
}
OrderStat <- function(f,n,r){
  function(y){
    factorial(n)/(factorial(r-1)*factorial(n-r))*f(y)*(fCDF(f)(y))^(r-1)*(1-fCDF(f)(y))^(n-r)
  }
}

#For normal distribution
n <- 20
df <- data.frame(seq(-5,5,by=.1))
colnames(df) <- "x"
df$y <- Vectorize(OrderStat(dnorm,n,1))(df$x)
ggplot(df,aes(x=x,y=y)) + geom_point()
#Order Statistics
os <- data.frame(1:n)
colnames(os) <- "r"
os$val <- sapply(os$r,function(z){
  integrate(function(x){x*Vectorize(OrderStat(dnorm,n,z))(x)},lower = -Inf,upper = Inf)[[1]]
  })
os
ggplot(os,aes(x=r,y=val)) + geom_bar(stat="identity", fill = "orange") +
  labs(title = paste("Order Statistics for n =",n), x = "rth Order Statistic", y = "Value of Order Statistic") +
  theme(plot.title = element_text(hjust = 0.5))

orderStats <- function(f,n) {
  sapply(1:n,function(z){
    integrate(function(x){x*Vectorize(OrderStat(f,n,z))(x)},lower = -Inf,upper = Inf)[[1]]
  })
}

orderStats(dnorm,10)
