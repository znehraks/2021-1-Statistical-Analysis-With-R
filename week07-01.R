rnorm(100,0,10)
plot(density(rnorm(100000,0,10)))
pnorm(0)
qnorm(0.5)
qnorm(0.9)

x=seq(0,16,length=100)
y=dnorm(x,mean=7.5, sd=1.5)
plot(x,y,type='l',
     xlab="Liters per day",
     ylab="Density",
     main="Water drunken by school children < 12 years old")

#4리터 이하의 물 마실 확률
pnorm(4, mean=7.5, sd=1.5, lower.tail = T)

#8리터 이상의 물 마실 확률
pnorm(8, mean=7.5, sd=1.5, lower.tail = F)
lower = 8
upper = 15
i = x>=lower & x<upper
polygon(c(lower, x[i], upper), c(0, y[i],0),col="red")
abline(h=0,col="red")

pb = round(pnorm(8, mean=7.5, sd=1.5, lower.tail=F),2)
pb
pb.results=paste("Cumluative probability of a child drinking > 8L/day",
                 pb, sep=":")
title(pb.results)

#기초 통계량
mean(1:5)
var(1:5)
sd(1:5)
fivenum(1:11)
summary(1:11)
x=factor(c("a","b","c","c","c","d","d"))
x
table(x)
which.max(table(x))
names(table(x))[3]

#emp 분석
emp = read.csv("employees_ex.csv")
emp$gender = as.factor(emp$gender)
str(emp)
hist(emp$incentive, breaks=50)
summary(emp$incentive)

hist(emp$incentive[emp$year==2007], breaks=50)
hist(emp$incentive[emp$year==2008], breaks=50)
hist(emp$incentive[emp$gender=="F"], breaks=50)
hist(emp$incentive[emp$gender=="M"], breaks=50)
hist(emp$incentive[emp$negotiated==F], breaks=50)
hist(emp$incentive[emp$negotiated==T], breaks=50)

#propagate
install.packages("propagate")
library("propagate")
set.seed(275)
observations = rnorm(10000,5)
distTested = fitDistr(observations)
distTested

#incentive
incentiveDis = fitDistr(emp$incentive)
incentiveDis

#frequency
table(c("a","b","a","d","e","d","a","c","a","b"))

CTable = data.frame(x=c("3","7","9","10"),
                    y=c("A1","B2","A1","B2"),
                    num = c(4,6,2,9))
CTable

xtabs(num~x, data=CTable)
xtabs(num~y, data=CTable)

temp = xtabs(num~x+y, data=CTable)

temp
addmargins(temp)

# contingency table: table(x,y)
str(Cars93)
Car_table_3 = with(Cars93, table(Type, Cylinders))
Car_table_3

# contingency table: xtabs(~x+y, data)
Car_table_4 = xtabs(~Type + Cylinders, data=Cars93)
Car_table_4
addmargins(Car_table_4)

#
install.packages("vcd")
library(vcd)
mosaic(Car_table_4, gp=gpar(fill=c("Red","blue")),
       direction = "v",
       main="Mosaic ploy of CarType vs. Type+Cylinders",
       main_gp = gpar(fontsize=10),
       gp_varnames=gpar(fontsize=10),
       gp_labels=gpar(fontsize=10))

# UCB Admission data
data("UCBAdmissions")
str(UCBAdmissions)
UCBAdmissions.df = as.data.frame(UCBAdmissions)
ucb_table1 = xtabs(Freq~Gender+Admit, data=UCBAdmissions.df)
ucb_table1
mosaic(ucb_table1,
       gp=gpar(fill=c("Red","Blue")),
       direction="v",
       main="Mosaic plot of UCB Admission vs. Gender")
      )
options("digit"=3)
ucb_table2 = prop.table(ucb_table1)
ucb_table2
chisq.test(ucb_table1)
