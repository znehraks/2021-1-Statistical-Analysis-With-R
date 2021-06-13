library(dplyr)
#데이터 로드 및 정제
ad = read.csv("ad.csv")
ad = ad %>% 
  select(-X)
str(ad)
attach(ad)
plot(ad)
#난수 생성 및 훈련데이터와 검정데이터 생성
set.seed(100)
trainingRowIndex = sample(1:nrow(ad), 0.6*nrow(ad))
trainingData = ad[trainingRowIndex,]
testData = ad[-trainingRowIndex,]

#다중선형회귀모델 생성(종속변수가 sales(판매량)이며 독립변수가 TV,radio,
#newspaper로 이루어진 모든 경우)
lmAd = lm(sales~., data=trainingData)
summary(lmAd)
plot(lmAd)

salesPred = predict(lmAd, testData)
salesPred

actuals_preds = data.frame(cbind(actuals = testData$sales, predict=salesPred))
actuals_preds[,1]
actuals_preds[,2]
head(actuals_preds)

cor_acc = cor(actuals_preds)
cor_acc

