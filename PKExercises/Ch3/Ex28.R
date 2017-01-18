df<-read.csv("Problem28.csv")
str(df)
df$Frequency<- as.character(df$Frequency)
df$Frequency<-gsub(",","",df$Frequency)
df$Frequency<- as.numeric(df$Frequency)
df$RFrequency<- df$Frequency/sum(df$Frequency)
df$Commute.Time<-as.character(df$Commute.Time)
df$Commute.Time<-gsub(" to <","-", df$Commute.Time)
for(i in 1:nrow(df)){
  c<- strsplit(df$Commute.Time[i], "-")[[1]]
  c<-as.numeric(c)
  df$Range[i]<-c[2]-c[1]
}

df$Density<- round(df$RFrequency/df$Range,5)
df$Commute.Time
df$Commute.Time<-factor(df$Commmute.Time, levels=df$Commute.Time)
ggplot(df, aes(x=Commute.Time,y=Density))+geom_bar(stat="identity")
