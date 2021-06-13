plot(10:1)
close.screen(all=TRUE)

split.screen(c(2,1))
split.screen(c(1,3), screen=2)

screen(1)
plot(10:1)
screen(4)
plot(1:10)
close.screen(all=TRUE)

dev.off()

#1. 산포도 그래프
x = seq(1,10,0.1)
y = exp(x)

plot(x,y, type = "l", col = "blue", lty=2)
title("Exponential Value")

data(ToothGrowth)
head(ToothGrowth)
dim(ToothGrowth)
plot(ToothGrowth)

#2 barchart
B_QTY = c(110,300,150,280,310)
S_QTY = c(180,200,210,190,170)
P_QTY = c(210,150,260,210,70)
B_TYPE2 = matrix(c(B_QTY,S_QTY,P_QTY),5,3)

B_TYPE2

barplot(B_TYPE2, main="Ball Type 별 시즌 판매량",xlab="Ball Type", ylab="판매량",
        beside=T,
        names.arg=c("BaseBall", "SoccerBall", "PingpongBall"),
        border="blue",col=rainbow(5),ylim=c(0,400))

legend(1,400,
       c("Season A","Season B","Season C","Season D","Season E"),
       cex=0.8, fill=rainbow(5))

barplot(t(B_TYPE2), main="시즌 별 BallType 판매량",xlab="Ball Type", ylab="판매량",
        beside=T,
        names.arg=c("A", "B", "C", "D", "E"),
        border="blue",col=rainbow(3),ylim=c(0,400))

barplot(t(B_TYPE2), main="시즌 별 BallType 판매량",xlab="Ball Type", ylab="판매량",
        #eside=T,
        names.arg=c("A", "B", "C", "D", "E"),
        border="blue",col=rainbow(3),ylim=c(0,1000))

legend(0,1000,
       c("BaseBall","SoccerBall","PingpongBall"),
       cex=0.8, fill=rainbow(3))

#3 dotchart
x = c(1:10)
dotchart(x,label=paste("Test", 1:10),pch=22)

#4 linechart

x = c(1,2,3,4,5,6,7,8,9)
y = x*2
z = x*3
plot(x,y,type="o")
points(x,z,pch="+")
lines(x,z,col="blue")

plot(x,y,type="o",ylim=c(0,30))
points(x,z,pch="*")
lines(x,z,col="dark red")

#5 histogram
b=c(1,2,1,4,3,5,4,5,3,2,5,6,7,2,8,5,9,3,5)
hist(b)

#6 Piechart
T_sales = c(210,110,400,550,700,130)
pie(T_sales, init.angle = 90, col=rainbow(length(T_sales)),
    main = "주간 매출 변동",
    labels = c("Mon","Tue", "Wed", "Thur", "Fri", "Sat"))
legend(1,1,c("Mon","Tue","Wed","Thur","Fri","Sat"),
       cex=0.8, fill=rainbow(length(T_sales)))

erase.scrren()

install.packages('plotrix')
library("plotrix")
week = c("Mon","Tue","Wed","Thur","Fri","Sat")
ratio=round(T_sales/sum(T_sales)*100,1)
label = paste(week,"\n", ratio, "%")
pie3D(T_sales, col=rainbow(length(T_sales)),
    main = "주간 매출 변동",
    labels = label,
    explode=0.1)
legend(-0.8,1,c("Mon","Tue","Wed","Thur","Fri","Sat"),
       cex=0.8, fill=rainbow(length(T_sales)))

#6 boxplot
A = c(110,300,150,280,310)
B = c(180,200,210,190,170)
C = c(210,150,260,210,70)
boxplot(A,B,C, col=c("yellow", "cyan", "green"), 
        names= c("Baseball", "Soccerball","Pingpongball"),
        horizontal=F)

dev.off()
data(iris)
plot(iris$Sepal.Width)
sepal.stat1 = boxplot(iris$Sepal.Width)
sepal.stat1

sepal.stat2 = boxplot(Sepal.Width ~ Species, data=iris,
                      col=c("blue","green","red"),
                      xlab="Iris Species")
sepal.stat2
boxplot(Sepal.Width ~ Species, data=iris, notch=T,
        col=c("blue","green","red"),
        xlab="Iris Species")

dev.off()

boxplot_hist = layout(matrix(c(1,2,2),3,1, byrow=TRUE))
layout.show(boxplot_hist)

boxplot(iris$Sepal.Length, horizontal = T, col = "green1")
hist(iris$Sepal.Length, xlim=c(4,8),col = "pink", freq = F)
lines(density(iris$Sepal.Length))

plot.new()
plot(-4:4, -4:4, type="n")
points(rnorm(200), rnorm(200), pch="+", col="red")

par(new=T)
points(rnorm(200), rnorm(200), pch="o", col="cyan")

x = c(1,1,1,2,2,2,2,2,2,3,3,4,5,6)
y = c(2,1,4,2,3,2,2,2,2,2,1,1,1,1)
zz = data.frame(x,y)
sunflowerplot(zz)

#8 Symbol
xx = c(1,2,3,4,5)
yy = c(2,3,4,5,6)
zz = c(10,50,100,20,10)
symbols(xx,yy,zz)

#3D
x1 = seq(-3,3,length=500)
x2 = seq(-4,4, length=600)
f = function(x1,x2){x1^2+x2^2+x1*x2}
y = outer(x1,x2,FUN=f)
persp(x1,x2,y)
