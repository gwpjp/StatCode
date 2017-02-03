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

#Now try to compute the value directly
OrderStat <- function(f,n,r){
  F <- function(z) {
    integrate(f,-Inf,z)[[1]]
  }
  function(y){
    factorial(n)/(factorial(r-1)*factorial(n-r))*f(y)*(F(y))^(r-1)*(1-F(y))^(n-r)
  }
}

#For exponential distribution
expDist <- function(x,l){.5*(exp(-l*x)+x/abs(x)*exp(-l*x))}
exp1 <- function(x){expDist(x,1)}
ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = exp1)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = OrderStat(exp1,10,1)) +
  stat_function(fun = function(x){10*expDist(10*x,1)},color = "red")


