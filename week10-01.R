#
str(sleep)
data = sleep
sleep2 = sleep[,-3]
tapply(sleep2$extra, sleep2$group, mean)

var.test(extra~group, sleep2)

t.test(extra~group, data = sleep2, paired=FALSE, var.equal=TRUE)

with(sleep, t.test(extra[group==1], extra[group==2], paired=TRUE))

#일표본 이항 검정
waiting_period = c(3,5,4,5.5,3.5,2.5,3,5,4.5,3,3.5)
above4.hrs=ifelse(waiting_period > 4, "yes","no")
above4hr.table = table(above4.hrs)
above4hr.table

binom.test(4, n=11, p=0.5)


#동전던지기 50%확인
prop.test(42,100)

#이표본 비율 검정
prop.test(c(45,55),c(100,90))

#남녀승진비율 검정
prop.test(c(16,63),c(430,1053))

#ANOVA
xx = c(1,2,3,4,5,6,7,8,9)
yy = c(1.09,2.12,2.92,4.06,4.90,6.08,7.01,7.92,8.94)
zz = c(1.10,1.96,2.98,4.09,4.92,6.10,6.88,7.97,9.01)

mydata = c(xx,yy,zz)

mydata

group = c(rep(1,9), rep(2,9), rep(3,9))
group

oneway.test(mydata~group, var=T)


#ANOVA- oneway
my_data = PlantGrowth
my_data
my_data$group = ordered(my_data$group, levels=c("ctrl","trt1","trt2"))

#compute summary
library(dplyr)
group_by(my_data,group) %>% 
  summarise(count = n(),
            mean = mean(weight, na.rm=T),
            sd = sd(weight, na.rm = T))

#visualize the three groups
#boxplot
boxplot(weight~group, data=my_data,
        xlab="Treatment", ylab="weight",
        frame=F, col=c("blue", "yellow", "red"))

#compute the analysis of variance
res.aov = aov(weight~group, data=my_data)
summary(res.aov)
