library(ggplot2)
library(dplyr)
tyre = read.csv("tyre.csv")
str(tyre)
summary(tyre)

ggplot(data=tyre, aes(x=Mileage, y=Brands))+geom_boxplot()

tyre %>% group_by(Brands) %>% 
  summarise(N=n(), Mean=mean(Mileage),
            Median = median(Mileage), Min=min(Mileage),
            Max=max(Mileage), SD=sd(Mileage))

boxplot(tyre$Mileage~tyre$Brands,
        main="Boxplot comaring Mileage of Four Brands",
        col=rainbow(4),
        horizontal = T)

tyres.aov = aov(Mileage~Brands, tyre)
summary(tyres.aov)
TukeyHSD(tyres.aov, conf.level=0.95)
