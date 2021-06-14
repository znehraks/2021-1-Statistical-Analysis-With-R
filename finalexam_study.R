#기말고사 공부용
#통계분석 차이

#패키지 모음
#propagate -- fitDistr()은 적합한 분포를 찾게해줌
library(ggplot2)

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
#
drug = read.csv("drug.csv")
drug
str(drug)
drug$fatigue = ordered(drug$fatigue, levels=c("low","med","high"))
boxplot(dose~fatigue, data=drug,
        xlab="fatigue",ylab="dose",
        col=c("blue","red","yellow"))
drug.aov = aov(dose~fatigue, data=drug)
summary(drug.aov)
plot(drug.aov)

drug_aov2 = update(drug.aov, subset(drug, patientID!=20))
summary(drug_aov2)

drug_aov3 = aov(dose~fatigue+gender, data=drug)
summary(drug_aov3)

#두 anova 모델 비교
#p-value가 0.05보다 크므로,차이가 없기에, 더 간단한 모델을 선택
anova(drug.aov, drug_aov3)

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
         order="FPC",#AOE, hclust, FPC, alphabet
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, diag=F)

#-----------------------------------------------------------------------------
#회귀분석
#-----------------------------------------------------------------------------
data(cars)
cor.test(cars$speed, cars$dist, method = "pearson")

attach(cars)
plot(speed,dist)
cor.test(speed,dist)

#선형회귀모델 생성
m = lm(dist~speed, cars)
#dist = -17.579 + 3.932*speed + e
m
summary(m)
plot(speed, dist)
abline(coef(m))
#선형회귀모델-예측
predict(m, newdata=data.frame(speed=3), interval = "confidence")
#-5.781869 = -17.599095+3.932*3

#예측 및 검정
set.seed(100)
#row indices for training data
trainingRowIndex = sample(1:nrow(cars), 0.8*nrow(cars))
#model training data
trainingData = cars[trainingRowIndex,]
trainingData
testData = cars[-trainingRowIndex,]
testData

lmMod = lm(dist~speed, data=trainingData)
lmMod
summary(lmMod)

distPred = predict(lmMod, testData)
distPred
#검정
actuals_preds = data.frame(cbind(actuals = testData$dist,
                                 predict=distPred))
head(actuals_preds)
correlation_accuracy = cor(actuals_preds)
correlation_accuracy

#키랑 몸무게 (몸무게로부터 키 예측)
reg = read.csv("regression.csv")
head(reg)
tail(reg)
cor(reg$height, reg$weight)

r=lm(reg$height~reg$weight)

plot(reg$weight, reg$height)
abline(coef(r))
summary(r)
#height = 70.9481 + 1.5218*weight + e
plot(r)

#다중선형회귀(독립변수가 2개 이상)
m=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data=iris)
summary(m)

iris$Species
m=lm(Sepal.Length~., data=iris)
summary(m)
model.matrix(m)[c(1,51,101),]
anova(m)

with(iris, plot(Sepal.Width, Sepal.Length, cex=.7, pch=as.numeric(Species)))
(m=lm(Sepal.Length~Sepal.Width + Species, data=iris))
coef(m)

abline(2.25,0.80, lty=1)
abline(2.25+1.45, 0.80, lty=2)
abline(2.25+1.94, 0.80, lty=3)

#다중선형회귀 상호작용
with(iris, plot(Species, Petal.Length, xlab="Species", ylab="Petal.Length"))
m2 = lm(Petal.Length~Petal.Width*Species, data=iris)
anova(m2)
summary(m2)
#Setosa 일때, 1.32+Petal.Width*0.54
#VersiColor 일때, 1.32+0.45+Petal.Width*0.54 + Petal.Width*1.32
#Virginica 일때, 1.32+2.91+Petal.Width*0.54 + Petal.Width*0.10

library(interactions)
interact_plot(m2,pred = "Petal.Width", modx="Species",plot.points = T)
#1.A,B,C와 그 상호작용을 모두 표현할 경우
#A+B+C+A:B+A:C+B:C+A:B:C
#A*B*C

x=1:1000
y=x^2+3*x+5+rnorm(1000)
y
lm(y~I(x^2)+x)

x=101:200
y=exp(3*x+rnorm(100))
lm(log(y)~x)

x=1:1000
y=log(x)+rnorm(1000)
lm(y~log(x))

#선형회귀 데이터 변환
time=c(1,5,15,30,60,120,240,480,720,1440,2880,5760,10080)
prop=c(0.84,0.71,0.61,0.56,0.54,0.47,0.45,0.38,0.36,0.26,0.20,0.16,0.08)
data=as.data.frame(cbind(time,prop))
data
m=lm(prop~time, data=data)
summary(m)
plot(data$time,data$prop)
abline(coef(m))
plot(m)
#곡선형을 띌땐 로그를 취하라
m2 = lm(prop~log(time), data=data)
summary(m2)
plot(m2)
plot(log(data$time),prop)
abline(coef(m2))

#몸무게와 뇌의 무게간 관계분석
library(MASS)
data(mammals)
head(mammals)
plot(mammals$body,mammals$brain)

m1 = lm(brain~body, data = mammals)
summary(m1)
plot(m1)

plot(mammals$body, mammals$brain, log="xy")

m2 = lm(log(brain)~log(body), data = mammals)
summary(m2)
par(mfrow=c(2,2), mar=c(2,3,1.5,0.5))
plot(m2)

#잔차비교
plot(density(m1$resid),main="m1")
plot(density(m2$resid),main="m2")

#2.Stepwise Algorithm
data("attitude")
m = lm(rating~., data=attitude)
summary(m)

#후진제거법
library(mlbench)
m2 = step(m, direction="backward")
m2
summary(m2)

#단계선택법
data("BostonHousing")
m=lm(medv~., data=BostonHousing)
m2=step(m, direction="both")
summary(m)
summary(m2)

library(leaps)
m=regsubsets(medv~., data=BostonHousing)
summary(m)

plot(m, scale="adjr2")
plot(m, scale="bic")
(bestpic = summary(m)$bic)
(min.bic = which.min(bestpic))
coef(m,min.bic)

#par(mfrow=c(1,1), mar=c(2,3,1.5,0.5))

#3.Dealing with Outliers
best_jump = c(5.30, 5.55, 5.47, 5.45, 5.07, 5.32, 6.15, 4.70, 5.22,
              5.77, 5.12, 5.77, 6.22, 5.82, 5.15, 4.92, 5.20, 5.42)
avg_takeoff = c(.09, .17, .19, .24, .16, .22, .09, .12, .09, .09, 
                .13, .16, .03, .50, .13, .04, .07, .04)
plot(avg_takeoff, best_jump)
jump_model = lm(best_jump~avg_takeoff)
abline(reg = jump_model, col="red")
summary(jump_model)
plot(jump_model)

#이상치 제거 실행!
best_jump2 = best_jump[-14]
avg_takeoff2 = avg_takeoff[-14]
jump_model2 = lm(best_jump2~avg_takeoff2)
plot(best_jump2~avg_takeoff2)
abline(jump_model2, col="red")
summary(jump_model2)
plot(jump_model2)

#오렌지 이상치
data("Orange")
Orange
plot(Orange$circumference ~ Orange$age)
#이상치 주입
Orange = rbind(Orange,
               data.frame(Tree=as.factor(c(6,6,6)),
                          age=c(118,484,664),
                          circumference=c(177,50,30)))
with(Orange,
     plot(Tree, circumference, xlab="tree",
          ylab="circumference"))
with(Orange, interaction.plot(age, Tree, circumference))
m = lm(circumference~age+I(age^2), data=Orange)
summary(m)
plot(m)
library(car)
#Bonferroni p value < 0.05
outlierTest(m)

data("airquality")
str(airquality)
head(airquality)
col1 = mapply(anyNA, airquality)
col1
for(i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"] = mean(airquality[which(airquality[,"Month"]
                                                  ==airquality[i,"Month"]),
                                            "Ozone"],na.rm=T)
  }
}

for(i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"] = mean(airquality[which(airquality[,"Month"]
                                                  ==airquality[i,"Month"]),
                                            "Solar.R"],na.rm=T)
  }
}
head(airquality)
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
airquality = normalize(airquality)
str(airquality)
head(airquality)
library(corrplot)
airquality_cor = cor(airquality)
corrplot(airquality_cor, method = "color",
         type="lower",addCoef.col = "black")
Y = airquality[,"Ozone"]
X = airquality[,"Solar.R"]

model1 = lm(Y~X)
model1
summary(model1)
plot(Y~X)
abline(model1, col="blue", lwd=2)

model2 = lm(Y~airquality$Wind)
summary(model2)
plot(Y~airquality$Wind)
abline(model2, col="blue", lwd=3)

airquality$forecast = predict(model1)
ggplot(data=airquality, aes(x=Ozone, y=forecast))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title="airquality linear regression model")

model3 = lm(airquality$Ozone~., data=airquality)
summary(model3)
m3=step(model3, direction="both")
summary(m3)


#-------------------------------------------------------------------------
#분류
#-------------------------------------------------------------------------
#의사결정나무
library(rpart)
library(rpart.plot)
m=rpart(Species~., data=iris)
m
prp(m, type=5, extra=2, digits=3)
table(iris$Species, predict(m, newdata = iris, type="class"))

#타이타닉
titanic = read.csv("titanic_clean.csv")
str(titanic)
#data cleansing
titanic$pclass = as.factor(titanic$pclass)
titanic$survived = factor(titanic$survived, levels=c(0,1))
str(titanic)

#understanding data: by plot
ggplot(titanic, aes(x=factor(pclass), fill=factor(sex)))+
  geom_bar(position="dodge")
ggplot(titanic, aes(x=factor(pclass), fill=factor(sex)))+
  geom_bar(position="dodge")+
  facet_grid(".~survived")

posn.j=position_jitter(0.3,0)
ggplot(titanic, aes(x=factor(pclass), y=age, col=factor(sex)))+
  geom_jitter(size=3, alpha=0.5, position=posn.j)+
  facet_grid(".~survived")
mosaicplot(survived~pclass+sex, main="pclass and sex",data = titanic,
           color=T)

library(Hmisc)
summary(survived~pclass + sex+ age + sibsp+parch+fare+embarked,
        data = titanic, method = "reverse")

library(rpart)
library(rpart.plot)

set.seed(100)
trainingRowIndex = sample(1:nrow(titanic), 0.8*nrow(titanic))
trainingData=titanic[trainingRowIndex,]
testData = titanic[-trainingRowIndex,]

dt1 = rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,
            data=trainingData)
prp(dt1, type=0, extra=2, digits=3)

#prediction
prediction = predict(dt1, testData, type="class")

#check accuracy
table(ifelse(testData$survived==prediction, "yes","no"))



#9주차 homework 독립성분석
library(MASS)
str(survey)
#손글씨를 어느 손으로 쓰는지와 박수를 칠 때 어느 손이 위로 가는지는 모두
#명목형 번수이므로 카이제곱검정을 이용해 독립성 여부를 판단한다.
#쓰는 손과 박수 별로 빈도수
WHnd_Clap = xtabs(~W.Hnd+Clap, data=survey)
WHnd_Clap
#표본의 수가 작으므로 카이제곱검정 보다는 피셔검정으로 재검정한다.
chisq.test(WHnd_Clap)
#p-value가 0.05보다 작으므로, 두 변수는 독립하지 않다(관계가 있다.)
fisher.test(WHnd_Clap)

ExerSmoke = xtabs(~Exer+Smoke, data=survey)
ExerSmoke
chisq.test(ExerSmoke)
fisher.test(ExerSmoke)

#10주차 homework 독립성 분석
dat = data(iris)
dat = iris
dat$size = ifelse(dat$Sepal.Length<median(dat$Sepal.Length),"small","big")
sizeSpecies = xtabs(~size+Species, data=dat)
sizeSpecies
#p-value가 0.05보다 작으므로, 독립이 아니다
chisq.test(sizeSpecies)

library(dplyr)
group = c(rep("Woman",9),rep("Man",9))
weight=c(38.9,61.2,73.3,21.8,63.4,64.6, 48.4, 48.8, 48.5,
         67.8, 60.0, 63.4, 76.0, 89.4, 73.3, 67.3, 61.3, 62.4)
data = as.data.frame(cbind(group,weight))
data$weight = as.double(data$weight)
womanWeight = data %>% filter(group=="Woman")
manWeight = data %>% filter(group=="Man")

#두 성별 모두 몸무게가 정규분포임
shapiro.test(womanWeight$weight)
shapiro.test(manWeight$weight)

#t-test를 통해 평균은 다르다고 할 수 있는지 검정
t.test(manWeight$weight, mu=mean(womanWeight$weight), alternative = "two.sided")
#p-value가 0.05 보다 작으므로 평균은 다르다고 할 수 있다.