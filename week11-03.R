drug = read.csv("drug.csv")
drug
str(drug)
drug$fatigue = ordered(drug$fatigue, levels=c("low","med","high"))
boxplot(dose~fatigue, data=drug,
        xlab="fatigue",ylab="dose",
        col = c("blue", "red", "yellow"))

drug.aov = aov(dose~fatigue, data=drug)
summary(drug.aov)

drug.aov02 = update(drug.aov, subset(drug,patientID != 20))
summary(drug.aov02)
plot(drug.aov02)

drug.aov3 = aov(dose~fatigue+gender, data=drug)
summary(drug.aov3)

anova(drug.aov, drug.aov3)
