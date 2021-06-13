cor(iris$Sepal.Width, iris$Sepal.Length)
cor(iris[,1:4])

symnum(cor(iris[,1:4]))

install.packages("corrgram")
library(corrgram)
corrgram(iris,upper.panel = panel.conf)

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method = "kendall")

#####################

data = as.data.frame(economics)
cor.test(data$unemploy, data$pce)

#####################
head(mtcars)
car_cor = cor(mtcars)
round(car_cor, 2)
install.packages("corrplot")
library(corrplot)
corrplot(car_cor)

corrplot(car_cor,
         method = "color", #method: circle, square.ellipse, number, shade, color, pie
         order="FPC", #order: AOE, hclust, FPC, alphabet
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45, diag = F)
