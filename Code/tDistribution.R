library(ggplot2)
#Generate normal distribution
set.seed(100)
N <- 1000000
samp <- as.data.frame(rnorm(N))
colnames(samp) <- "value"
ggplot(samp,aes(samp)) + geom_histogram(bins = 500, aes(y=..count../max(..count..))) + labs(x ="x",y = "")
#Take L samples of size n from the normal distribution
n <- 5
L <- 10000
mn <- array(dim = L)
for (i in 1:L) {
  s <- sample(samp$value,n)
  mn[i] <- mean(s)/sd(s)*sqrt(n)
}
mn <- as.data.frame(mn)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=(2*pi)^-.5*..count../max(..count..)),bins=200) + labs(x = "Sample Mean", y= "") +
  stat_function(fun = function(x){dt(x,n-1)}, color = "green") + 
  stat_function(fun = dnorm, color = "blue")
#Repeating for another n value
n <- 10
L <- 10000
mn <- array(dim = L)
for (i in 1:L) {
  s <- sample(samp$value,n)
  mn[i] <- mean(s)/sd(s)*sqrt(n)
}
mn <- as.data.frame(mn)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=(2*pi)^-.5*..count../max(..count..)),bins=200) + labs(x = "Sample Mean", y= "") +
  stat_function(fun = function(x){dt(x,n-1)}, color = "green") + 
  stat_function(fun = dnorm, color = "blue")