library(ggplot2)
mtcars
table(mtcars$cyl)
ggplot(mtcars,aes(cyl)) + geom_bar(width = 1)
ggplot(mtcars,aes(cyl)) + geom_dotplot()
diamonds
ggplot(diamonds,aes(price)) + geom_histogram()
ggplot(diamonds,aes(price)) + geom_histogram(color = "darkblue", fill = "white") + xlab("Price") + ylab("Count")
ggplot(diamonds,aes(price)) + geom_histogram(color = "darkblue", fill = "white") + xlab("Price") + ylab("Count") + 
  facet_wrap(~cut, nrow = 2)
ggplot(diamonds,aes(price,fill=cut)) + geom_histogram() + xlab("Price") + ylab("Count")
