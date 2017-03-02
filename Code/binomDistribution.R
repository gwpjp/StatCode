library(ggplot2)
#Take L samples of size n from the binomial distribution with probability p
p <- .3
n <- 10
L <- 50000
mn <- array(dim = L)
for (i in 1:L) {
  s <- rbinom(n,1,p)
  mn[i] <- mean(s)
}
mn <- as.data.frame(mn)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..)),bins=10*n) + 
  labs(x = "Mean of Sample Proportion", y= "") + 
  stat_function(fun = function(x){dbinom(n*x,size = n,prob = p)/dbinom(n*p,size = n,prob = p)}, color = "green") + 
  stat_function(fun = function(x){dnorm(x,mean = p, sd = (p*(1-p)/n)^.5)/dnorm(p,mean = p, sd = (p*(1-p)/n)^.5)}, color = "blue")
#With a different scale from 0 to n
p <- .3
n <- 10
L <- 50000
mn <- array(dim = L)
for (i in 1:L) {
  s <- rbinom(n,1,p)
  mn[i] <- sum(s)
}
mn <- as.data.frame(mn)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..)),bins=10*n) + 
  labs(x = "Mean of Sample Proportion", y= "") + 
  stat_function(fun = function(x){dbinom(x,size = n,prob = p)/dbinom(n*p,size = n,prob = p)}, color = "green") + 
  stat_function(fun = function(x){dnorm(x,mean = n*p, sd = (p*(1-p)*n)^.5)/dnorm(n*p,mean = n*p, sd = (p*(1-p)*n)^.5)}, color = "blue")
#Repeating for another n value
p <- .3
n <- 50
L <- 10000
mn <- array(dim = L)
for (i in 1:L) {
  s <- rbinom(n,1,p)
  mn[i] <- mean(s)
}
mn <- as.data.frame(mn)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..)),bins=200) + 
  labs(x = "Mean of Sample Proportion", y= "") + 
  stat_function(fun = function(x){dbinom(x = n*x,size = n,prob = p)/dbinom(n*p,size = n,prob = p)}, color = "green") + 
  stat_function(fun = function(x){dnorm(x,mean = p, sd = (p*(1-p)/n)^.5)/dnorm(p,mean = p, sd = (p*(1-p)/n)^.5)}, color = "blue")
