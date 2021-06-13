#60152572(유정민) 중간고사
library(ggplot2)
library(dplyr)
library(readxl)

#전처리
welfare = read.csv("welfare.csv")
job_name = read_excel("jobname21.xlsx")
welfare$code_job = as.integer(welfare$code_job)
job_name$code_job = as.integer(job_name$code_job)

job_name = as.data.frame(job_name)
welfare
job_name

str(welfare)
str(job_name)


#1
welfare$income = ifelse(welfare$income == 9999.00, NA, welfare$income)
welfare

#남성 income 및 summary, boxplot
male_income = welfare %>% 
  select(gender,income) %>% 
  filter(gender == 1)
male_income = na.omit(male_income)
male_income
summary(male_income)
ggplot(data=male_income, aes(x=gender, y=income))+geom_boxplot()

#여성 income 및 summary, boxplot
female_income = welfare %>% 
  select(gender,income) %>% 
  filter(gender == 2)
female_income = na.omit(female_income)
female_income
summary(female_income)
ggplot(data=female_income, aes(x=gender, y=income))+geom_boxplot()

#남성 평균: 312, 최댓값: 1853, 최솟값: 0
#여성 평균: 161.8, 최댓값: 2400, 최솟값: 0

#2
welfare$age = 2021-welfare$birth+1
welfare_divorce = welfare %>% 
  mutate(age_group = ifelse(age>=60,"노년(old)",
                            ifelse(age<30,"초년(young)",
                                   "중년(middle)"))) %>% 
  filter(marriage == 3) %>% 
  group_by(age_group) %>% 
  summarise(n = n())

welfare_divorce = as.data.frame(welfare_divorce)
ggplot(data=welfare_divorce, aes(x=age_group, y=n))+geom_col()

#노년에서 가장 높다

#3
union = left_join(welfare, job_name, by="code_job")
union_male_top10 = union %>% 
  filter(gender == 1) %>% 
  filter(!is.na(code_job)) %>% 
  group_by(code_job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

result = left_join(union_male_top10,job_name,by="code_job")
result = as.data.frame(result)
result
ggplot(data=result, aes(x=n,y=reorder(job_name,n)))+geom_col()

#작물 재배 종사자, 자동차 운전원, 경영 관련 사무원
#영업 종사자, 매장 판매 종사자, 제조 관련 단순 종사자
#청소원 및 환경미화원, 건설 및 광업 단순 종사자, 건물 관리원 및 검표원
#행정 사무원 순으로 높다.

#4
fence = function(original, wrapper){
  res = c()
  return(c(wrapper,original,wrapper))
}
fence(c("i","love","R"),"?")

#5
airq = airquality
airq

#5-1
top_temp = airq %>% 
  arrange(desc(Temp)) %>% 
  select(Month, Day) %>% 
  head(1)
top_temp
#8월 28일이다.


#5-2
wind_june = airq %>% 
  filter(Month == 6) %>% 
  arrange(desc(Wind)) %>%
  select(Wind) %>% 
  head(1)
wind_june  
#20.7


#5-3
mean_july = airq %>% 
  group_by(Month) %>% 
  summarise(mean = mean(Temp)) %>% 
  filter(Month == 7)
mean_july
#83.9


#5-4
ozone_over_100 = airq %>% 
  filter(Ozone > 100)
ozone_over_100
#7일이다.