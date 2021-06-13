#기말고사 공부용
#통계분석 차이

#패키지 모음
#propagate -- fitDistr()은 적합한 분포를 찾게해줌


#어린이 4리터 이하 물
x=seq(0,16,length=100)
y=dnorm(x,mean=7.5, sd=1.5)
plot(x,y,type="l",
     xlab="Liters per day",
     ylab="Density",
     main="Water drunken by school children < 12 years old")
pnorm(4, mean = 7.5, sd=1.5, lower.tail = T)

#어린이 8리터 이상 물
lower = 8
upper = 15
i = x>=lower & x<upper
polygon(c(lower, x[i], upper), c(0,y[i],0),col = "red")
abline(h=0, col="red")

pb=round(pnorm(8, mean=7.5, sd=1.5, lower.tail = F),2)
pb
pb.results = paste("cumulative probability of a child drinking > 8L/day",
                   pb, sep=":")
title(pb.results)

#기초통계량
mean(1:5)
var(1:5)
sd(1:5)
summary(1:11)

x=factor(c("a","b","c","c","c","c","d","d"))
x
table(x)
which.max(table(x))
names(table(x))[
  which.max(table(x))]

#연봉협상 데이터분석
data = read.csv("employees_ex.csv")
data
str(data)
summary(data)
hist(data$incentive, breaks = 50)
summary(data$incentive)
#조건에따른 incentive data 분포 분석
hist(data$incentive[data$year==2007], breaks=50)
hist(data$incentive[data$year==2008], breaks=50)
hist(data$incentive[data$gender=="F"], breaks=50)
hist(data$incentive[data$gender=="M"], breaks=50)
#협상여부가 영향을 미침
hist(data$incentive[data$negotiated==F], breaks=50)
hist(data$incentive[data$negotiated==T], breaks=50)
library(propagate)
set.seed(275)
observations = rnorm(10000,5)
distTested = fitDistr(observations)
distTested

incentive_dist = fitDistr(data$incentive)
incentive_dist

#교차분석
table(c("a","b","a","d","e","a","c","a","b","d"))
CTable = data.frame(x=c("3","7","9","10"),
                    y=c("A1","B2","A1","B2"),
                    num=c(4,6,2,9))
CTable
xtabs(num~x, data=CTable)
xtabs(num~y, data=CTable)
temp = xtabs(num~x+y, data=CTable)

addmargins(temp,margin=1)
addmargins(temp,margin=2)
addmargins(temp)

str(Cars93)
Car_table_3 = with(Cars93, table(Type, Cylinders))
Car_table_3

Car_table_4 = xtabs(~Type + Cylinders, data=Cars93)
Car_table_4

addmargins(Car_table_4)

library(vcd)
mosaic(Car_table_4,
       gp=gpar(fill=c("Red","blue")),
       direction = "v",
       main="Mosaic plot of CarType vs. Type+Cylinders")

data("UCBAdmissions")
str(UCBAdmissions)
UCBAdmissions.df = as.data.frame(UCBAdmissions)
str(UCBAdmissions)
ucb_table1 = xtabs(Freq~Gender+Admit, data=UCBAdmissions.df)
ucb_table1
mosaic(ucb_table1,
       gp=gpar(fill=c("Red","Blue")),
       direction = "v",
       main = "Mosaic plot of UCB Admission vs. Gender")
options("digit"=3)
ucb_table2 = prop.table(ucb_table1)
ucb_table2
chisq.test(ucb_table1)

#-----------------------------------------------------------------------------
#차이 분석
#-----------------------------------------------------------------------------
#카이제곱분포(학생들의 성별에 따른 운동량에 차이가 있는가)
#H0: 성별과 운동은 독립이다, H1: 성별과 운동은 독립이 아니다.
data("survey")
str(survey)
survey
SexExer = xtabs(~Sex+Exer, data = survey)
SexExer
#자유도는 성별이 2레벨, 운동량이 3레벨이므로 (2-1)(3-1)=df=2
#p-value가 0.05보다 크므로 귀무가설채택(성별과 운동은 독립)
chisq.test(SexExer)

#카이제곱 검정(비율)
child1 = c(5,11,1)
child2 = c(4,7,3)
Toy = cbind(child1, child2)
rownames(Toy) = c("car", "truck", "doll")
Toy
chisq.test(Toy)
fisher.test(Toy)

#KS검정(두 데이터의 분포가 다른지 검정)
x = rnorm(50)
x
y = runif(30)
y
ks.test(x,y)

#shapiro wilk test(데이터가 정규분포를 따르는지 검정)
shapiro.test(rnorm(100,mean = 5,sd=3))
library(UsingR)
data(cfb)
str(cfb)
shapiro.test(cfb$INCOME)
#시각화 (히스토그램)
hist(cfb$INCOME, breaks = 100)
#Kernel Density Plot, y:probability(freq=False)
hist(cfb$INCOME, freq=F, breaks = 100,
     main="Kernel Density Plot of cfb$INCOME")
#not Normal Dist, King of F dit (Skewed Left)
#Q-QPlot: qqnorm(), qqline()
qqnorm(cfb$INCOME)
qqline(cfb$INCOME)

#T-test 한 집단 혹은 두 집단의 평균을 비교하는 Parametic Test(모수적 감정법)
#모수적 방법: 정규성을 갖는다는 모수적 특성을 이용
#비모수적 방법: 순위척도를 사용함(모수 사용불가능): 
#순위합검정: 자료를 크기 순으로 배열하여 순위를 매긴 다음 순위의 합을 통해 차이 비교
#t.test
t.test(
  #일표본(표본이 하나)t검정인 경우 x에만,이표본이면 x,y모두에 숫자벡터지정
  x,
  y=NULL,
  alternative=c("two.sided", "less", "greater"), #대립가설
  mu=0, #모집단의 평균
  paired=F, #paired t-test의 경우는 TRUE
  var.equal = F, #이표본 검정에서 두 집단의 분산이 같은지 여부
  conf.level = 0.95,
)
#T-test
#A회사의 건전지 수명시간이 1000시간일때, 무작위로 뽑은 10개의 건전지에 대한
#수명은 다음과 같다
a=c(980,1008,968,1032,1012,996,1021,1002,996,1017)
#정규성 검정을 통해 정규분포를 이룬다고 알게됨
shapiro.test(a)
t.test(a, mu=1000, alternative = "two.sided")

#3-1반 학생들의 수학 평균성적은 55점
#0교시 수업 시행 후, 시험성적은 다음과 같다
a=c(58,49,39,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
shapiro.test(a)
t.test(a, mu=55, alternative = "greater")

#표본이 두개인 T-test
pre = c(13.2,8.2,10.9,14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
post = c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
shapiro.test(pre)
shapiro.test(post)
#두 데이터의 분산 분석
var.test(pre,post)
t.test(pre,post, paired=F, var.equal=T)
#결론. 딱히 차이가 없다

#표본이 두 개인 경우
A = c(rep(5,8), rep(4,11), rep(3,9), rep(2,2), rep(1,3))
B = c(rep(5,4), rep(4,6), rep(3,10), rep(2,8), rep(1,4))
A
#A는 정규분포가 아님
shapiro.test(A)
B
#B는 정규분포가 아님
shapiro.test(B)
#정규성을 띄지 않을 때
wilcox.test(A,B, exact = F, correct = F)

#수면제 효과도 분석
str(sleep)
#Extra 수면 시간 증가량
#Group 그룹 ID
#ID 환자 ID
sleep
#개인차 고려하지 않을 때 개개인을 고려X 단순 group1과 group2 비교
sleep2 = sleep[,-3]
sleep2
tapply(sleep2$extra, sleep2$group, mean)
var.test(extra~group, sleep2)
t.test(extra~group, data=sleep2, paired=F,var.equal=T)
   
#개인별 고려o 1번은 1번끼리.. paired t-test
with(sleep, t.test(extra[group==1], extra[group==2],paired=T))

#비율검정
a=c(3,5,4,5.5,3.5,2.5,3,5,4.5,3,3.5)
above4.hrs = ifelse(a >4, "yes","no")
above4.hrs
above4hr.table = table(above4.hrs)
above4hr.table
binom.test(4, n=11, p=0.5)

prop.test(42,100, 0.5)

#이표본 비율 검정
prop.test(c(45,55), c(100,90))

prop.test(c(16,63),c(430,1053))

#------------------------------------------------------------------------
#ANOVA
#------------------------------------------------------------------------
#정규성가정
#분산의 동질성 가정
#관찰의 독립성 가정
xx = c(1,2,3,4,5,6,7,8,9)
yy = c(1.09, 2.12, 2.92, 4.06, 4.90, 6.08, 7.01, 7.92, 8.94)
zz = c(1.10, 1.96, 2.98, 4.09, 4.92, 6.10, 6.88, 7.97, 9.01)
mydata = c(xx,yy,zz)
mydata
group = c(rep(1,9), rep(2,9), rep(3,9))
group
#p-value가 0.05보다 크므로, 그룹간 차이가 없다
oneway.test(mydata~group, var=T)

my_data = PlantGrowth
my_data
my_data$group = ordered(my_data$group, levels=c("ctrl","trt1","trt2"))

#compute summary
library(dplyr)
group_by(my_data,group) %>% 
  summarise(
    count=n(),
    mean=mean(weight, na.rm=T),
    sd=sd(weight,na.rm=T)
  )
#visualize the three groups
#boxplot
boxplot(weight~group, data=my_data,
        xlab="Treatment", ylab="Weight",
        frame=F, col=c("blue", "yellow", "red"))

#Compute the analysis of variance
res.aov = aov(weight~group, data=my_data)
summary(res.aov)

#TukeyHSD
TukeyHSD(res.aov)

tyre = read.csv("tyre.csv")
str(tyre)
tyre %>% group_by(Brands) %>% 
  summarise(N=n(),
            Mean=mean(Mileage),
            Median=median(Mileage),
            Min=min(Mileage),
            Max=max(Mileage),
            SD=sd(Mileage)
  )
boxplot(tyre$Mileage~tyre$Brands,
        main="Boxplot comparing Mileage of Four Brands",
        col=rainbow(4),
        horizontal = T)  

tyres.aov = aov(Mileage~Brands, tyre)
summary(tyres.aov)
TukeyHSD(tyres.aov)

#--------------------------------------------------------------------------
#상관분석
#--------------------------------------------------------------------------
iris
cor(iris$Sepal.Width, iris$Sepal.Length)
cor(iris[,1:4])

#상관분석 시각화
symnum(cor(iris[,1:4]))
library(corrgram)
corrgram(iris)
corrgram(iris,upper.panel = panel.conf)

cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method = "pearson")
cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method = "spearman")
cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method = "kendall")
#
economics = as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)
#
head(mtcars)
car_cor = cor(mtcars)
round(car_cor,2)
library(corrplot)
corrplot(car_cor)
corrplot(car_cor, method="color",#circle, square,ellipse,number,shade,color,pie
         type="lower",
         order="hclust",#AOE, hclust, FPC, alphabet
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, diag=F)
