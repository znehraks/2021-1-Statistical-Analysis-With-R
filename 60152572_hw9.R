#Q1
library(MASS)
data("survey")
str(survey)
handClap = xtabs(~W.Hnd+Clap, data=survey)
handClap
addmargins(handClap)
chisq.test(handClap)
fisher.test(handClap) 
#p-value가 0.05보다 작으므로 대립가설을 채택하며, 대립가설이
#"서로 차이가 있다"를 의미하므로 두 변수간의 관계가 있다.

#Q2
ExerSmoke = xtabs(~Exer+Smoke, data=survey)
ExerSmoke
addmargins(ExerSmoke)
chisq.test(ExerSmoke)
fisher.test(ExerSmoke) 
#p-value가 0.05보다 크므로 귀무가설을 채택하며, 귀무가설이
#"서로 차이가 없다"를 의미하므로 두 변수간의 관계는 없다.