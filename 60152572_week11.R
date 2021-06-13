#가설: 2021년 2월,3월,4월의 데이터를 바탕으로 
#일별 1호선 지하철 승하차 총 인원수는 월별로 평균의 차이가 있을 것이다.

library(dplyr)
library(ggplot2)
Feb = read.csv("CARD_SUBWAY_MONTH_202102.csv")
March = read.csv("CARD_SUBWAY_MONTH_202103.csv")
April = read.csv("CARD_SUBWAY_MONTH_202104.csv")

str(Feb)
str(March)
str(April)
#분석에 있어서 1,2,3,4호선의 row만을 필터링하고, 불필요한 컬럼은 제거함
#해당 날짜의 총 승하차인원이 필요하므로, 그룹핑하여 저장함
Feb2 = Feb %>% 
  filter(노선명 %in%c("1호선")) %>% 
  mutate(total = 승차총승객수+하차총승객수) %>% 
  select(-등록일자,-승차총승객수,-하차총승객수) %>% 
  group_by(사용일자, 노선명) %>% 
  summarise(mean_total = mean(total))

March2 = March %>% 
  filter(노선명 %in%c("1호선")) %>% 
  mutate(total = 승차총승객수+하차총승객수) %>% 
  select(-등록일자,-승차총승객수,-하차총승객수) %>% 
  group_by(사용일자, 노선명) %>% 
  summarise(mean_total = mean(total))

April2 = April %>% 
  filter(노선명 %in%c("1호선")) %>% 
  mutate(total = 승차총승객수+하차총승객수) %>% 
  select(-등록일자,-승차총승객수,-하차총승객수) %>% 
  group_by(사용일자, 노선명) %>% 
  summarise(mean_total = mean(total))

Feb2 = as.data.frame(Feb2)
March2 = as.data.frame(March2)
April2 = as.data.frame(April2)
Feb2$month = "2"
March2$month = "3"
April2$month = "4"

#월별 같은 호선끼리 묶기위해 데이터를 호선별로 나눔
Feb2_line1 = Feb2 %>% filter(노선명 == "1호선")
March2_line1 = March2 %>% filter(노선명 == "1호선")
April2_line1 = April2 %>% filter(노선명 == "1호선")

#호선별로 새로운 데이터로 전처리
line1 = bind_rows(bind_rows(Feb2_line1,March2_line1),April2_line1)

line1
str(line1)

#boxplot으로 시각화하여, 대략 어떤 분포를 가지는지 확인함
ggplot(data=line1, aes(x=month, y=mean_total))+geom_boxplot()


#정규성검정
#정규성을 갖지 않음
shapiro.test(Feb2_line1$mean_total)
shapiro.test(March2_line1$mean_total)
shapiro.test(April2_line1$mean_total)

#분산의 동질성 검정
#등분산성을 만족함
var.test(Feb2_line1$mean_total, March2_line1$mean_total)
var.test(March2_line1$mean_total, April2_line1$mean_total)
var.test(Feb2_line1$mean_total, April2_line1$mean_total)

#관찰의 독립성 검정
#데이터 특성상 보장된다고 가정

#mean_total을 종속변수로, 월을 독립변수로하여 크루스칼 왈리스 검정 시행함
kruskal.test(mean_total~month,line1)

#subway_aov = aov(mean_total~month, line1)
#summary(subway_aov)
#plot(subway_aov)
#결과 확인
#P값이 0.05보다 현저히 작으므로 차이는 유의미하다는 결론도출
#
