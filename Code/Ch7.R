library(ggplot2)
#Example 7.17
df717 <- as.data.frame(read.table("../AddedData/ex7_17.txt"))
colnames(df717) <- "val"
df717$val <- sort(df717$val)
ggplot(df717,aes(sample=val))+stat_qq(color = "orange") + 
  labs(title="Example 7.17",x="Normal Score",y="Egg Weight (g)") +
  theme(plot.title = element_text(hjust = 0.5))

str(gg717)
gg717$mapping
