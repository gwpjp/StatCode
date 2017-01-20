library(ggplot2)
#Example 4.9
df <- read.csv("ex4_9.txt")
colnames(df) <- "Bachelors"
summary(df)
df$Bachelors <- sort(df$Bachelors)

#Basic plot of data
ggplot(df,aes(x=Bachelors,y=factor(1))) + geom_point() + ylab("")
#Adjusting in various ways
ggplot(df,aes(x=Bachelors,y=factor(1))) + geom_point(alpha = .4) + ylab("")
ggplot(df,aes(x=Bachelors,y=factor(1))) + geom_jitter(height = .05) + ylab("")

#Making a box plot
ggplot(df,aes(x=factor(1),y=Bachelors)) + geom_boxplot() + xlab("")
ggplot(df,aes(x=factor(1),y=Bachelors)) + geom_boxplot(width = .1) + xlab("") + coord_flip()


#Example 4.13 (Modified for 2016-2017 Salaries)
#Data courtesy of hoopshype.com
dfSal <- read.csv("../AddedData/Ex4_13.csv",na.strings = c("NA",""))
dfSal <- na.omit(dfSal)
str(dfSal)
ggplot(dfSal,aes(x=Team,y=X2016.17)) + geom_boxplot() + coord_flip() + ylab("2016-2017 Salaries")
library(scales)
ggplot(dfSal,aes(x=Team,y=X2016.17,fill=Team)) + geom_boxplot() + coord_flip() + 
  labs(y = "2016-2017 Salaries", title = "NBA 2016-2017 Salaries by Team") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = dollar)

#Example 4.18
x <- c(1:10)
mean(x)
sd(x)
(x - mean(x))/sd(x)
scale(x)
#Accounting
(45000-46000)/1500 #-0.6667
#Marketing
(43000-42500)/1000 #0.5

#Calculating density, probabilities (i.e p-score/p-value), quantiles
#Density
dnorm(0) - 1/sqrt(2*pi)*exp(-0^2/2)
dnorm(seq(-3,3,by=.1)) - 1/sqrt(2*pi)*exp(-seq(-3,3,by=.1)^2/2)
#Probabilites
pnorm(0)
pnorm(1) 
pnorm(1) - integrate(dnorm,-Inf,1)[[1]]
round(pnorm(seq(-3,3,by=.1)) - sapply(seq(-3,3,by=.1),function(x){integrate(dnorm,-Inf,x)[[1]]}),5)
pnorm(1) - pnorm(-1) #68%
pnorm(2) - pnorm(-2) #95%
pnorm(3) - pnorm(-3) #99.7%
#Quantiles
qnorm(.5) #0
qnorm(pnorm(1)) #1
round(qnorm(pnorm(seq(-3,3,by=.1))) - seq(-3,3,by=.1),5)

#Comparing biased variance to unbiased variance
set.seed(100)
N <- 1000000
samp <- as.data.frame(rnorm(N))
head(samp)
colnames(samp) <- "value"
ggplot(samp,aes(samp)) + geom_histogram(bins = 500, aes(y=..count../max(..count..))) + labs(x ="x",y = "")
n <- 50
L <- 1000
mn <- array(dim = L)
var <- array(dim = L)
var2 <- array(dim = L)
for (i in 1:L) {
  s <- sample(samp$value,n)
  mn[i] <- mean(s)
  var[i] <- var(s) #original var is the unbiased variance
  var2[i] <- var[i]*(n-1)/n
}
mn <- as.data.frame(mn)
var <- as.data.frame(var)
var2 <- as.data.frame(var2)
ggplot(mn,aes(x = mn)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Mean", y= "") 
ggplot(var,aes(var)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Variance", y = "", title="Unbiased Variance" ) + theme(plot.title = element_text(hjust = 0.5))
ggplot(var2,aes(var2)) + geom_histogram(aes(y=..count../max(..count..))) + labs(x = "Variance", y = "", title="Biased Variance" ) + theme(plot.title = element_text(hjust = 0.5))

