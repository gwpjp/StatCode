library(ggplot2)
library(scales)
#Example 5.1
df51 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_01.txt")
colnames(df51) <- c("Observation","GradRate","Expenditure")
ggplot(df51,aes(x=Expenditure,y=GradRate)) + geom_point(color="orange") + 
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
  labs(title = "Example 5.2 Residuals", x="Student-related Expenditure", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(labels = dollar) +
  geom_hline(aes(yintercept = 0))
#One more way to calculate r
df52$SSTO <- (df52$Astringency - mean(df52$Astringency))^2
df52$SSResid <- (residuals(lm(Astringency ~ Tannin,df52)))^2
(1-sum(df52$SSResid)/sum(df52$SSTO))^.5
#Standard Deviation
summary(lm(Astringency ~ Tannin,df52))
(sum(df52$SSResid)/(nrow(df52)-2))^.5

#Generalized Models
#Example 5.14
df54 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_04.txt")
colnames(df54) <- c("AgeGroup","RepAge","Time")
df54$AgeGroup <- gsub("\x96","-",df54$AgeGroup)
df54$AgeGroup <- gsub("'","",df54$AgeGroup)
ggplot(df54,aes(x=RepAge,y=Time)) + geom_point(color="orange") + labs(x = "Representative Age (yr)", y = "Average Finish Time (min)")
lm(Time ~ poly(RepAge,2,raw=TRUE),df54)
l <- lm(Time ~ RepAge + I(RepAge^2),df54)
summary(l)
ggplot(df54,aes(x=RepAge,y=Time)) + geom_point(color="orange") + 
  labs(title = "Example 5.14", x = "Representative Age (yr)", y = "Average Finish Time (min)") +
  stat_function(fun = function(x){l[[1]][3]*x^2+l[[1]][2]*x+l[[1]][1]},color="blue") + 
  theme(plot.title = element_text(hjust = 0.5)) 
residuals(l)
ggplot(df54,aes(x=RepAge,y=residuals(l))) + geom_point(color="red") +
  labs(title = "Example 5.14 Residuals", x="Representative Age (yr)", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(aes(yintercept = 0))

#Example 5.15
df515 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_15.txt")
colnames(df515) <- c("Sunflower","Weight")
ggplot(df515,aes(x=Sunflower,y=Weight)) + geom_point(color="orange") + 
  labs(x = "Sunflower Meal (%)", y = "Average Fish Weight (g)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5))
l <- lm(Weight ~ poly(Sunflower,3,raw=TRUE),df515)
summary(l)
ggplot(df515,aes(x=Sunflower,y=Weight)) + geom_point(color="orange") + 
  labs(x = "Sunflower Meal (%)", y = "Average Fish Weight (g)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(fun = function(x){l[[1]][4]*x^3+l[[1]][3]*x^2+l[[1]][2]*x+l[[1]][1]},color="blue") 
ggplot(df515,aes(x=Sunflower,y=residuals(l))) + geom_point(color="red") +
  labs(title = "Example 5.15 Residuals", x="Sunflower Meal (%)", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(aes(yintercept = 0))
