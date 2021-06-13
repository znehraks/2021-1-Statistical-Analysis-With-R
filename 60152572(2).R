library(mlbench)
library(corrplot)
library(ggplot2)
#airquality 데이터를 로드함
data("airquality")

#이상치 확인 및 제거
col1 = mapply(anyNA, airquality)
col1
for(i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]=mean(airquality[which(airquality[,"Month"]
                                                == airquality[i,"Month"]),
                                          "Ozone"],na.rm=T)
  }
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]=mean(airquality[which(airquality[,"Month"]
                                                == airquality[i,"Month"]),
                                          "Solar.R"],na.rm=T)
  }
}

#정규화
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
airquality = normalize(airquality)
str(airquality)
head(airquality)

#상관관계 파악
airquality_cor = cor(airquality)
corrplot(airquality_cor, method = "color",
         type="lower", addCoef.col = "black")

#1차 선형회귀모델 확인 (태양광과 오존의 모델)
Y = airquality[,"Ozone"]
X = airquality[,"Solar.R"]

model1 = lm(Y~X)
model1
plot(Y~X)
abline(model1, col="blue", lwd=3)
summary(model1)

#1차 예측모델
airquality$forecast1 = predict(model1)
ggplot(data=airquality, aes(x=Ozone, y=forecast1)) +
  geom_point()+
  geom_smooth(method = lm)+
  labs(title="airquality linear regression model")

#2차 선형회귀모델 확인 (바람과 오존의 모델)
Y = airquality[,"Ozone"]
X = airquality[,"Wind"]

model2 = lm(Y~X)
model2
plot(Y~X)
abline(model2, col="blue", lwd=3)
summary(model2)

#2차 예측모델
airquality$forecast2 = predict(model2)
ggplot(data=airquality, aes(x=Ozone, y=forecast2)) +
  geom_point()+
  geom_smooth(method = lm)+
  labs(title="airquality linear regression model")


#Ozone에 다른 변수들이 미치는 영향을 확인하기 위해, 1차로 모델을 생성
m=lm(Ozone~., data=airquality)
#Intercept, Solar.R, Wind, Temp, Month가 유의미한 영향력을 끼치는 것으로 확인
summary(m)

 #전진선택, 후진제거, 단계적선택법 모두를 이용하여 모델을 만든결과,
#AIC가 679.71이며, Solar.R, Wind, Temp, Month로 이루어진 모델이 가장 최상의
#모델로 결정됨
#Ozone ~ Solar.R + Wind + Temp + Month
m2=step(m, direction = "both")
summary(m2)

m3=step(m, direction = "forward")
summary(m3)

m4=step(m, direction= "backward")
summary(m4)
