#Histogram
win.graph()
hist(iris$Sepal.Width, col='green3')

#BarChart
win.graph()
barplot(summary(iris$Species),col=c('green3','pink','blue'))

#Boxplot
win.graph()
boxplot(Sepal.Length~Species,data=iris,horizontal=
TRUE,col=c("red3","blue3","green3"))
legend(y=1.5 ,x=6.5 ,legend=c("setosa","versicolor",
"virginica"),pch=c(2 ,2 ,2) ,col=c("red3","blue3","green3"))
