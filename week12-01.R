data(cars)
cars
plot(cars$speed, cars$dist)
cor.test(cars$speed, cars$dist, method = "pearson")

m = lm(cars$dist ~ cars$speed, cars)
m

summary(m)

full = lm(cars$dist ~ cars$speed, data=cars)
reduced = lm(cars$dist~1, data=cars)

anova(reduced,full)

plot(m)
plot(cars$speed, cars$dist)
abline(coef(m))

data(cars)
(m=lm(cars$dist~cars$speed, data=cars))
predict(m, newdata = data.frame(speed=3), interval = "confidence")
coef

set.seed(100)
trainingRowIndex = sample(1:nrow(cars), 0.8*nrow(cars))
trainingData = cars[trainingRowIndex,]
testData = cars[-trainingRowIndex,]

lmMod = lm(dist~speed, data=trainingData)
summary(lmMod)

distPred = predict(lmMod, testData)
distPred

actuals_preds = data.frame(cbind(actuals = testData$dist, predict=distPred))
head(actuals_preds)

cor_acc = cor(actuals_preds)
cor_acc


reg = read.csv("regression.csv")
head(reg)
tail(reg)
plot(reg)
plot(reg$weight, reg$height)

cor(reg$height, reg$weight)
r = lm(reg$height~reg$weight)
r
abline(r)
summary(r)

plot(r)

#multi
attach(iris)
iris_m=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data=iris)
iris_m=lm(Sepal.Length~.,data = iris)
summary(iris_m)
plot(iris_m)
model.matrix(iris_m)[c(1,51,101),]
anova(iris_m)

with(iris, plot(Sepal.Width, Sepal.Length, cex=.7, pch=as.numeric(Species)))
(m=lm(Sepal.Length~Sepal.Width+Species, data=iris))
coef(m)

abline(2.25,0.80,lty=1)
abline(2.25+1.45,0.80,lty=2)
abline(2.25+1.94,0.80,lty=3)

