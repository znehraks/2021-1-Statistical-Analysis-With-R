#1 iris 데이터 분석
dat = iris
dat$size = ifelse(dat$Sepal.Length < median(dat$Sepal.Length),"small","big")
dat
speciesSize = xtabs(~Species+size, data = dat)
chisq.test(speciesSize)
#p-value가 0.05보다 작으므로 H0:Species와 size는 독립이다를 기각하므로
#iris의 종별로 사이즈의 차이가 있음을 알 수 있다.

#2 몸무게비교
group = c("Woman","Woman","Woman","Woman","Woman","Woman","Woman","Woman","Woman",
         "Man","Man","Man","Man","Man","Man","Man","Man","Man")

weight = c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5, 
           67.8, 60.0, 63.4, 76.0, 89.4,73.3,67.3,61.3,62.4)

#문제에 있는 데이터프레임을 생성함
data = data.frame(group=group, weight=weight)

#그룹별 몸무게의 평균을 구함
tapply(data$weight, data$group, mean)

#두 그룹의 몸무게의 분포의 분산이 같은지 검정함
#p값이 0.1714가 나오므로 0.05보다 크기에 귀무가설(두 집단의 분산에 차이가 없다를 채택함)
var.test(weight~group, data)

#두 그룹에 대해 T검정을 시행함
#p값이 0.01327이 나오므로 0.05보다 작기에 
#대립가설(두 집단의 평균의 차이는 0이 아니다를 채택함)
t.test(weight~group, data = data, paired=FALSE, var.equal=TRUE)

#결론: 여성과 남성의 몸무게의 평균은 다르다고 할 수 있다.