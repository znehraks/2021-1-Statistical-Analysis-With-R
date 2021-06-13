library(ggplot2)
library(dplyr)
mpg
#1
x = mpg$cyl
y = mpg$hwy
plot(x,y, main = "cyl, hwy Relation", xlab="cyl", ylab="hwy")
#대체로 음의 상관 관계를 가진다.

#2
x = mpg$cty
y = mpg$hwy
plot(x,y,main="cty,hwy Relation", xlab="cty", ylab="hwy")
#대체로 양의 상관 관계를 가진다.

#3
x = mpg %>% filter(class=="suv") %>% 
  group_by(manufacturer) %>% summarise(mean_c=mean(cty)) %>% 
  arrange(desc(mean_c)) %>% 
  head(5)
y = x$manufacturer
z = x$mean_c
barplot(z,beside = T,names.arg = y, col = rainbow(5),ylim = c(0,20))

#4
boxplot(mpg$hwy ~ mpg$trans, main="구동 방식 별 고속도로 연비",
        col= rainbow(10))

