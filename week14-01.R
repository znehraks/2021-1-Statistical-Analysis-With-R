install.packages("leaps")
install.packages("car")
library(car)
library(leaps)
library(mlbench)
data("BostonHousing")
m=regsubsets(medv~.,data=BostonHousing)
m
summary(m)

plot(m, scale = "adjr2")
plot(m, scale = "bic")
(bestbic = summary(m)$bic)
(min.bic = which.min(bestbic))
coef(m,min.bic)

data("Orange")
Orange
Orange = rbind(Orange,
               data.frame(Tree=as.factor(c(6,6,6)),
                          age=c(118,484,664),
                          circumference=c(177,50,30)))

with(Orange,
     plot(Tree, circumference, xlab="tree",
          ylab="circumference"))
with(Orange, interaction.plot(age, Tree, circumference))
m=lm(circumference ~ age+I(age^2), data=Orange)
summary(m)
plot(m)
outlierTest(m)


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(ggplot2)
m=rpart(Species~.,data=iris)
m
prp(m, type=5, extra=2, digits=3)

table(iris$Species, predict(m, newdata=iris, type="class"))


titanic = read.csv("titanic_clean.csv", header=T, sep=",")
str(titanic)

titanic$pclass=as.factor(titanic$pclass)
titanic$name=as.character(titanic$name)
titanic$ticket=as.character(titanic$ticket)
titanic$cabin=as.character(titanic$cabin)
titanic$survived=factor(titanic$survived, levels=c(0,1))

ggplot(titanic, aes(x=factor(pclass), fill=factor(sex)))+
  geom_bar(position = "dodge")
ggplot(titanic, aes(x=factor(pclass), fill=factor(sex)))+
  geom_bar(position = "dodge") +
  facet_grid(".~survived")

posn.j=position_jitter(0.3,0)
ggplot(titanic, aes(x=factor(pclass), y=age, col=factor(sex)))+
  geom_jitter(size=3, alpha=0.5, position=posn.j)+
  facet_grid(".~survived")

mosaicplot(survived~pclass+sex, main="pclass and sex", data=titanic,
           color = T)

library(Hmisc)
summary(survived~pclass + sex + age + sibsp + parch + fare + embarked,
        data=titanic, method = "reverse")

library(rpart)
library(rpart.plot)

set.seed(100)
trainingRowIndex = sample(1:nrow(titanic), 0.8*nrow(titanic))
trainingData = titanic[trainingRowIndex,]
testData = titanic[-trainingRowIndex,]

dt1 = rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,
            data=trainingData)
prp(dt1, type=0, extra=2, digits=4)

prediction = predict(dt1, testData, type="class")

table(ifelse(testData$survived == prediction, "yes", "no"))
