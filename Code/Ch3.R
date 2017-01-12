library(ggplot2)
str(mtcars)
head(mtcars)
ggplot(mtcars,aes(x=cyl,fill=factor(gear))) + geom_histogram(binwidth = 1,position="dodge") + 
  scale_fill_discrete(name="Gears",breaks=c(3, 4, 5)) + xlab("# of Cylinders") + ylab("Count")

#Ex 1
library(xlsx)
ex1 <- read.xlsx("Ex3_1.xlsx","Sheet1") #Read in the sheet naively
ex1 #This is not right!
ex1 <- read.xlsx("Ex3_1.xlsx","Sheet2") #Read in only a data sheet
ex1 #This looks right but we need the other columns
ex1$rf.Students <- ex1$Students/sum(ex1$Students)
ex1$rf.Parents <- ex1$Parents/sum(ex1$Parents)
ex1 #This is right!
ex1 <- read.xlsx("Ex3_1.xlsx","Sheet1",startRow = 2) #Read in the original sheet ignoring the first row
ex1 #This is right too!
ex1$Ideal.Distance <- factor(ex1$Ideal.Distance,levels = c("Less than 250 miles","250 to 500 miles","500 to 1000 miles","More than 1000 miles"))
ex1
#But the data is not in the right form for using ggplot
library(reshape2) 
ex1[,1:3]
ex1v2 <- melt(ex1[,1:3],"Ideal.Distance")
ex1v2
dcast(ex1v2, Ideal.Distance ~ variable)
#A bar graph with stacked bars
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=variable)) + geom_bar(stat="identity")
prefix <- "../Graphs/Ch3/Ex1/"
ggsave(filename = paste(prefix,"1-SimpleBarGraphStacked.jpeg",sep = ""))
#A bar graph with side-by-side bars
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=variable)) + geom_bar(stat="identity",position="dodge")
ggsave(filename = paste(prefix,"2-SimpleBarGraphSide.jpeg",sep = ""))
#A nicer looking bar graph
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=variable)) + geom_bar(stat="identity",position="dodge") +
  scale_fill_discrete(name="Respondent",breaks=c("Students","Parents"),labels=c("Student","Parent")) + 
  labs(x="Ideal Distance", y="Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(prefix,"3-NiceBarGraph.jpeg",sep = ""))
#Nice looking, but compact
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=variable)) + geom_bar(stat="identity",position="dodge") +
  scale_fill_discrete(name="Respondent",breaks=c("Students","Parents"),labels=c("Student","Parent")) + 
  labs(x="Ideal Distance", y="Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5),legend.position=c(.9,.9))
ggsave(filename = paste(prefix,"4-NiceBarGraph(Compact).jpeg",sep = ""))
#Faceting w/ colors as before
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=variable)) + geom_bar(stat="identity",position="dodge")  + 
  labs(x="Ideal Distance", y="Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5)) + facet_grid(facets = variable ~ .)
ggsave(filename = paste(prefix,"5-NiceBarGraph(Facets).jpeg",sep = ""))
#Faceting w/ new colors
ggplot(ex1v2,aes(x=Ideal.Distance,y=value,fill=Ideal.Distance)) + geom_bar(stat="identity",position="dodge")  + 
  labs(x="Ideal Distance", y="Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5)) + facet_grid(facets = variable ~ .)
ggsave(filename = paste(prefix,"6-NiceBarGraph(Facets).jpeg",sep = ""))


#Creating a pie chart
ex1v3 <- melt(ex1[,c(1,4:5)],"Ideal.Distance")
ex1v3$variable <- ex1v2$variable #hack to get labels correct
ex1v3
#First put together a bar graph of what we want
ggplot(ex1v3,aes(x=variable,y=value,fill=Ideal.Distance)) + geom_bar(stat="identity") +
  labs(x="Respondent",y="Relative Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name="Ideal Distance") 
ggsave(filename = paste(prefix,"7-NiceBarGraph(RF).jpeg",sep = ""))
#Do the same thing by faceting since we will want 2 pie charts
ggplot(ex1v3,aes(x=factor(""),y=value,fill=Ideal.Distance)) + geom_bar(stat="identity") + 
  labs(x="",y="Relative Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks = element_blank()) + 
  scale_fill_discrete(name="Ideal Distance") +
  facet_grid(facets = ~ variable)
ggsave(filename = paste(prefix,"8-NiceBarGraph(RF-Facets).jpeg",sep = ""))

#Make pie chart
ggplot(ex1v3,aes(x=factor(""),y=value,fill=Ideal.Distance)) + geom_bar(stat="identity") + coord_polar(theta="y") +
  labs(x="",y="Relative Frequency",title="How Far is Far Enough?") +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks = element_blank()) + 
  scale_fill_discrete(name="Ideal Distance") +
  facet_grid(facets = ~ variable) 
ggsave(filename = paste(prefix,"9-PieChart(Facets).jpeg",sep = ""))
