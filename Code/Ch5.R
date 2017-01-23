library(ggplot2)
library(scales)
#Example 5.1
df51 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_01.txt")
colnames(df51) <- c("Observation","GradRate","Expenditure")
ggplot(df51,aes(x=Expenditure,y=GradRate)) + geom_point(color="red") + 
  labs(title = "Example 5.1", x="Student-related Expenditure", y = "Graduation Rate") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(labels = dollar)
summary(lm(GradRate ~ Expenditure,df51))
ggplot(df51,aes(x=Expenditure,y=GradRate)) + geom_point(color="orange") + 
  labs(title = "Example 5.1", x="Student-related Expenditure", y = "Graduation Rate") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(labels = dollar) +
  geom_smooth(method = "lm",se = FALSE) 

summary(lm(GradRate ~ Expenditure,df51))$r.squared
(summary(lm(GradRate ~ Expenditure,df51))$r.squared)^.5
#An alternative way to calculate r
cov(df51$Expenditure,df51$GradRate)/(sd(df51$Expenditure)*sd(df51$GradRate))
#Looking the residuals
residuals(lm(GradRate ~ Expenditure,df51))
ggplot(df51,aes(x=Expenditure,y=residuals(lm(GradRate ~ Expenditure,df51)))) + geom_point(color="red") +
  labs(title = "Example 5.1 Residuals", x="Student-related Expenditure", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(labels = dollar) + 
  geom_hline(aes(yintercept = 0))
#One more way to calculate r
df51$SSTO <- (df51$GradRate - mean(df51$GradRate))^2
df51$SSResid <- (residuals(lm(GradRate ~ Expenditure,df51)))^2
(1-sum(df51$SSResid)/sum(df51$SSTO))^.5
#Standard Deviation
summary(lm(GradRate ~ Expenditure,df51))
(sum(df51$SSResid)/(nrow(df51)-2))^.5
   
#Example 5.2
df52 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_02.txt")
colnames(df52) <- c("Tannin","Astringency")
ggplot(df52,aes(x=Tannin,y=Astringency)) + geom_point(color="orange") + 
  labs(title = "Example 5.2", x="Tannin Concentration", y = "Perceived Astringency") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(df52,aes(x=Tannin,y=Astringency)) + geom_point(color="orange") + 
  labs(title = "Example 5.2", x="Tannin Concentration", y = "Perceived Astringency") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "lm", se = FALSE)
summary(lm(Astringency ~ Tannin,df52))
residuals(lm(Astringency ~ Tannin,df52))
summary(lm(Astringency ~ Tannin,df52))$r.squared
(summary(lm(Astringency ~ Tannin,df52))$r.squared)^.5
#An alternative way to calculate
cov(df52$Tannin,df52$Astringency)/(sd(df52$Tannin)*sd(df52$Astringency))
#Looking the residuals
residuals(lm(Astringency ~ Tannin,df52))
ggplot(df52,aes(x=Tannin,y=residuals(lm(Astringency ~ Tannin,df52)))) + geom_point(color="red") +
  labs(title = "Example 5.1 Residuals", x="Student-related Expenditure", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(labels = dollar) +
  geom_hline(aes(yintercept = 0))
#One more way to calculate r
df52$SSTO <- (df52$Astringency - mean(df52$Astringency))^2
df52$SSResid <- (residuals(lm(Astringency ~ Tannin,df52)))^2
(1-sum(df52$SSResid)/sum(df52$SSTO))^.5
#Standard Deviation
summary(lm(Astringency ~ Tannin,df52))
(sum(df52$SSResid)/(nrow(df52)-2))^.5
