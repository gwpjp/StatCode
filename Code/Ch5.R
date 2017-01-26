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
l <- lm(GradRate ~ Expenditure,df51)
summary(l)
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

#------------------------------------
#5.4:Generalized Models 
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

#Example 5.17
df517 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_17.txt")
colnames(df517) <- c("pH","Hg")
ggplot(df517,aes(x=pH,y=Hg)) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = expression(paste("Blood Mercury Level (",mu,"g/g)")), title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(df517,aes(x=pH,y=Hg)) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Blood Mercury Level (\u00B5g/g)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5))
#Using a transformation
ggplot(df517,aes(x=pH,y=log(Hg))) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Log(Blood Mercury Level)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5))
l517 <- lm(I(log10(Hg)) ~ pH,df517)
l517
summary(l517)
ggplot(df517,aes(x=pH,y=log10(Hg))) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Log(Blood Mercury Level)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm",se = FALSE)
ggplot(df517,aes(x=pH,y=log10(Hg))) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Log(Blood Mercury Level)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(fun = function(x){l517[[1]][2]*x+l517[[1]][1]},color = "blue",size=1)
#Back to the original function
ggplot(df517,aes(x=pH,y=Hg)) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Blood Mercury Level (\u00B5g/g)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(fun = function(x){10^(l517[[1]][2]*x+l517[[1]][1])},color = "blue",size=1)
#Another way
n517 <- nls(Hg ~ I(a*10^(b*pH)),data=df517,start=list(a=.1,b=.1))
summary(n517)
val517 <- coefficients(n517)
ggplot(df517,aes(x=pH,y=Hg)) + geom_point(color="orange") + 
  labs(x = "Lake pH", y = "Blood Mercury Level (\u00B5g/g)", title = "Example 5.15") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(fun = function(x){val517[1]*10^(val517[2]*x)},color = "blue",size=1)

#-------------------------
#5.5: Logistic Regression
#Example 5.20
df520 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_20.txt", col.names = c("Size","Cannibalism"))
ggplot(df520,aes(x=Size,y=Cannibalism)) + geom_point(color="orange",alpha = .4) +
  labs(x = "Size Difference (mm)", y = "Cannibalism", title = "Example 5.20") + 
  theme(plot.title = element_text(hjust = 0.5))
g520 <- glm(Cannibalism ~ Size,family = binomial,data = df520)
summary(g520)
val520 <- coefficients(g520)
ggplot(df520,aes(x=Size,y=Cannibalism)) + geom_point(color="orange",alpha = .4) +
  labs(x = "Size Difference (mm)", y = "Cannibalism", title = "Example 5.20") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(fun = function(x){exp(val520[1] + val520[2]*x)/(1 + exp(val520[1] + val520[2]*x))},color = "blue")
#by Transformation
t520 <- as.data.frame(table(df520)[,"1"]/(table(df520)[,"0"] + table(df520)[,"1"]))
colnames(t520) <- c("CannRate")
t520$Size <- as.numeric(rownames(t520))
t520$Total <- as.data.frame((table(df520)[,"0"] + table(df520)[,"1"]))
rownames(t520) <- NULL
colnames(t520[,3]) <- "Total"
t520
#Note: This tranformation will fail because the Cannibalism rate can be 0.
ggplot(t520,aes(x=Size,y=log(CannRate/(1-log(CannRate))))) + geom_point(color = "orange") 

#Example 5.22
df522 <- read.csv("../BookData/Chapter\ 5\ Examples/ex5_22.txt", col.names = c("Rain","Call"))
ggplot(df522,aes(x=Rain,y=Call)) + geom_point(color="orange") +
  labs(x = "Rainfall (mm)", y = "Call Rate", title = "Example 5.22") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(df522,aes(x=Rain,y=log(Call/(1-Call)))) + geom_point(color="orange") +
  labs(x = "Rainfall (mm)", y = "(Call Rate)'", title = "Example 5.22") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm",se = FALSE)
l522 <- lm(log(Call/(1-Call)) ~ Rain,df522)
summary(l522)
val522 <- coefficients(l522)
ggplot(df522,aes(x=Rain,y=Call)) + geom_point(color="orange") +
  labs(x = "Rainfall (mm)", y = "Call Rate", title = "Example 5.22") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_function(fun = function(x){exp(val522[1] + val522[2]*x)/(1+exp(val522[1] + val522[2]*x))},color = "blue",width = 1.5 )


