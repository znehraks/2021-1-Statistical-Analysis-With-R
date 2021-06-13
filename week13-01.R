with(iris, plot(Species, Petal.Length, xlab="Species", ylab="Petal.Length"))
m=lm(Petal.Length~Petal.Width*Species, data=iris)
anova(m)
summary(m)
install.packages("interactions")
library(interactions)
interact_plot(m,pred="Petal.Width",modx="Species", plot.points = TRUE)

x=1:1000
y = x^2+3*x+5+rnorm(1000)
lm(y~I(x^2)+x)

x=101:200
y=exp(3*x+rnorm(100))
lm(log(y)~x)

x=101:200
y=log(x)+rnorm(100)
lm(y~log(x))


library(MASS)
data("mammals")
head(mammals)
attach(mammals)
plot(body, brain)

model1 = lm(brain~body)
summary(model1)
plot(model1)

with(mammals, plot(body, brain, log = "xy"))

model2 = lm(log(brain)~log(body))
summary(model2)

par(mfrow=c(2,2), mar=c(2,3,1.5,0.5))
plot(model2)

plot(density(model1$resid), main="model1")
plot(density(model2$resid), main="model2")

install.packages("mlbench")
library(mlbench)
data("attitude")
m=lm(rating~., data=attitude)
m2=step(m, direction = "backward")
summary(m2)

data("BostonHousing")
m=lm(medv~., data=BostonHousing)
m2=step(m,direction="both")
m2